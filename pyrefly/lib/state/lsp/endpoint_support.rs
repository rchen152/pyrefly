/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::CompletionItemTextEdit;
use lsp_types::TextEdit;
use pyrefly_build::handle::Handle;
use pyrefly_python::ast::Ast;
use pyrefly_python::module::Module;
use pyrefly_python::module_path::ModulePathDetails;
use pyrefly_python::module_path::ModuleStyle;
use pyrefly_python::symbol_kind::SymbolKind;
use ruff_python_ast::AnyNodeRef;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprList;
use ruff_python_ast::ExprSet;
use ruff_python_ast::ExprStringLiteral;
use ruff_python_ast::ExprTuple;
use ruff_python_ast::Keyword;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use serde_json::to_string;
use vec1::Vec1;

use super::DefinitionMetadata;
use super::FindDefinitionItemWithDocstring;
use crate::lsp::wasm::completion::RankedCompletion;
use crate::state::state::Transaction;

mod cache;
mod provenance;

pub(crate) use cache::EndpointModuleCache;

const ENDPOINT_ROUTE_DECORATORS: [&str; 3] = ["route", "api_route", "websocket"];
const ENDPOINT_ROUTE_NEEDLES: [&str; 11] = [
    ".get(",
    ".post(",
    ".put(",
    ".delete(",
    ".patch(",
    ".options(",
    ".head(",
    ".trace(",
    ".route(",
    ".api_route(",
    ".websocket(",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EndpointMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Options,
    Head,
    Trace,
    WebSocket,
}

impl EndpointMethod {
    fn from_attr(attr: &str) -> Option<Self> {
        match attr {
            "get" => Some(Self::Get),
            "post" => Some(Self::Post),
            "put" => Some(Self::Put),
            "delete" => Some(Self::Delete),
            "patch" => Some(Self::Patch),
            "options" => Some(Self::Options),
            "head" => Some(Self::Head),
            "trace" => Some(Self::Trace),
            "websocket" => Some(Self::WebSocket),
            _ => None,
        }
    }

    fn from_literal(value: &str) -> Option<Self> {
        Self::from_attr(&value.to_ascii_lowercase())
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Get => "GET",
            Self::Post => "POST",
            Self::Put => "PUT",
            Self::Delete => "DELETE",
            Self::Patch => "PATCH",
            Self::Options => "OPTIONS",
            Self::Head => "HEAD",
            Self::Trace => "TRACE",
            Self::WebSocket => "WEBSOCKET",
        }
    }
}

struct EndpointDefinition {
    path: String,
    methods: Option<Vec<EndpointMethod>>,
    module: Module,
    definition_range: TextRange,
    docstring_range: Option<TextRange>,
}

struct EndpointCallContext {
    literal: ExprStringLiteral,
    method: EndpointMethod,
}

struct EndpointDecoratorSpec {
    path: String,
    methods: Option<Vec<EndpointMethod>>,
}

fn endpoint_decorator_spec(expr: &Expr) -> Option<EndpointDecoratorSpec> {
    let Expr::Call(call) = expr else {
        return None;
    };
    let callee = call_callee_name(call)?;
    if let Some(method) = EndpointMethod::from_attr(callee) {
        return Some(EndpointDecoratorSpec {
            path: endpoint_path_from_call(call)?,
            methods: Some(vec![method]),
        });
    }
    if ENDPOINT_ROUTE_DECORATORS.contains(&callee) {
        let path = endpoint_path_from_call(call)?;
        let methods = endpoint_methods_from_call(call).or_else(|| {
            if keyword_arg_expr(call, &["methods", "method"]).is_some() {
                None
            } else {
                Some(vec![EndpointMethod::Get])
            }
        });
        return Some(EndpointDecoratorSpec { path, methods });
    }
    None
}

fn call_callee_name(call: &ExprCall) -> Option<&str> {
    match call.func.as_ref() {
        Expr::Attribute(attr) => Some(attr.attr.as_str()),
        Expr::Name(name) => Some(name.id.as_str()),
        Expr::Call(call) => call_callee_name(call),
        _ => None,
    }
}

fn endpoint_path_from_call(call: &ExprCall) -> Option<String> {
    if let Some(Expr::StringLiteral(lit)) = call.arguments.args.first() {
        return Some(lit.value.to_string());
    }
    string_literal_kw_arg(call, &["path", "url", "route"]).map(|lit| lit.value.to_string())
}

fn endpoint_methods_from_call(call: &ExprCall) -> Option<Vec<EndpointMethod>> {
    methods_from_expr(keyword_arg_expr(call, &["methods", "method"])?)
}

fn methods_from_expr(expr: &Expr) -> Option<Vec<EndpointMethod>> {
    match expr {
        Expr::StringLiteral(lit) => {
            EndpointMethod::from_literal(&lit.value.to_string()).map(|method| vec![method])
        }
        Expr::List(ExprList { elts, .. })
        | Expr::Tuple(ExprTuple { elts, .. })
        | Expr::Set(ExprSet { elts, .. }) => {
            let methods = elts
                .iter()
                .filter_map(|elt| {
                    if let Expr::StringLiteral(lit) = elt {
                        EndpointMethod::from_literal(&lit.value.to_string())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            if methods.is_empty() {
                None
            } else {
                Some(methods)
            }
        }
        _ => None,
    }
}

fn keyword_arg_expr<'a>(call: &'a ExprCall, names: &[&str]) -> Option<&'a Expr> {
    call.arguments
        .keywords
        .iter()
        .find_map(|Keyword { arg, value, .. }| {
            arg.as_ref()
                .filter(|identifier| names.contains(&identifier.id.as_str()))
                .map(|_| value)
        })
}

fn string_literal_kw_arg(call: &ExprCall, names: &[&str]) -> Option<ExprStringLiteral> {
    match keyword_arg_expr(call, names)? {
        Expr::StringLiteral(lit) => Some(lit.clone()),
        _ => None,
    }
}

fn endpoint_call_context(
    transaction: &Transaction,
    handle: &Handle,
    nodes: &[AnyNodeRef<'_>],
    position: TextSize,
) -> Option<EndpointCallContext> {
    for node in nodes {
        let AnyNodeRef::ExprCall(call) = node else {
            continue;
        };
        let Expr::Attribute(attr) = call.func.as_ref() else {
            continue;
        };
        let Some(method) = EndpointMethod::from_attr(attr.attr.as_str()) else {
            continue;
        };
        let literal = match call.arguments.args.first() {
            Some(Expr::StringLiteral(literal)) => literal.clone(),
            _ => match string_literal_kw_arg(call, &["url"]) {
                Some(literal) => literal,
                None => continue,
            },
        };
        let mut parts = literal.value.iter();
        let Some(part) = parts.next() else {
            continue;
        };
        if parts.next().is_some() || !part.content_range().contains_inclusive(position) {
            continue;
        }
        if provenance::is_test_client(transaction, handle, attr.value.range()) {
            return Some(EndpointCallContext { literal, method });
        }
    }
    None
}

/// Decode only the typed prefix; the edit replaces the complete literal.
fn literal_prefix(
    module: &Module,
    literal: &ExprStringLiteral,
    position: TextSize,
) -> Option<String> {
    let part = literal.value.iter().next()?;
    let opener = module.code_at(TextRange::new(
        part.range().start(),
        part.content_range().start(),
    ));
    let quote = opener.find(['\'', '"'])?;
    let prefix = module.code_at(TextRange::new(part.range().start(), position));
    let closed_prefix = format!("{prefix}{}", &opener[quote..]);
    let Expr::StringLiteral(prefix) = Ast::parse_expr(&closed_prefix, TextSize::new(0)).ok()?
    else {
        return None;
    };
    Some(prefix.value.to_string())
}

fn module_might_define_endpoints(module: &Module) -> bool {
    ENDPOINT_ROUTE_NEEDLES
        .iter()
        .any(|needle| module.contents().contains(needle))
}

fn normalize_path(path: &str) -> &str {
    path.split('?').next().unwrap_or(path)
}

fn split_segments(path: &str) -> Vec<&str> {
    normalize_path(path)
        .trim_end_matches('/')
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect()
}

fn is_param_segment(segment: &str) -> bool {
    (segment.starts_with('{') && segment.ends_with('}'))
        || (segment.starts_with('<') && segment.ends_with('>'))
}

fn path_matches(pattern: &str, candidate: &str) -> bool {
    let pattern_segments = split_segments(pattern);
    let candidate_segments = split_segments(candidate);
    pattern_segments.len() == candidate_segments.len()
        && pattern_segments
            .iter()
            .zip(candidate_segments)
            .all(|(pattern, candidate)| is_param_segment(pattern) || *pattern == candidate)
}

fn method_matches(methods: &Option<Vec<EndpointMethod>>, request: EndpointMethod) -> bool {
    methods
        .as_ref()
        .is_none_or(|methods| methods.contains(&request))
}

impl Transaction<'_> {
    fn collect_endpoint_definitions(&self) -> Vec<EndpointDefinition> {
        let mut endpoints = Vec::new();
        for handle in self.handles() {
            let Some(module) = self.get_module_info(&handle) else {
                continue;
            };
            if module.path().style() == ModuleStyle::Interface {
                continue;
            }
            match module.path().details() {
                ModulePathDetails::FileSystem(_)
                | ModulePathDetails::Memory(_)
                | ModulePathDetails::Namespace(_) => {}
                _ => continue,
            }
            if !module_might_define_endpoints(&module) {
                continue;
            }
            let ast = self.get_ast(&handle);
            let summary = self.endpoint_module_cache().get(&module, ast.as_deref());
            endpoints.extend(summary.routes.iter().map(|route| EndpointDefinition {
                path: route.spec.path.clone(),
                methods: route.spec.methods.clone(),
                module: module.dupe(),
                definition_range: route.definition_range,
                docstring_range: route.docstring_range,
            }));
        }
        endpoints
    }

    pub(crate) fn add_endpoint_completions(
        &self,
        handle: &Handle,
        nodes: &[AnyNodeRef<'_>],
        position: TextSize,
        completions: &mut Vec<RankedCompletion>,
    ) {
        let Some(context) = endpoint_call_context(self, handle, nodes, position) else {
            return;
        };
        let Some(module_info) = self.get_module_info(handle) else {
            return;
        };
        let Some(prefix) = literal_prefix(&module_info, &context.literal, position) else {
            return;
        };
        let mut suggestions = BTreeMap::new();
        for endpoint in self.collect_endpoint_definitions() {
            if method_matches(&endpoint.methods, context.method)
                && endpoint.path.starts_with(&prefix)
            {
                suggestions
                    .entry(endpoint.path)
                    .or_insert_with(|| format!("{} endpoint", context.method.as_str()));
            }
        }
        for (label, detail) in suggestions {
            let new_text = to_string(&label).expect("Serializing an endpoint string cannot fail");
            completions.push(RankedCompletion::new(CompletionItem {
                label,
                detail: Some(detail),
                kind: Some(CompletionItemKind::Value),
                text_edit: Some(CompletionItemTextEdit::TextEdit(TextEdit {
                    range: module_info.to_lsp_range(context.literal.range()),
                    new_text,
                })),
                ..Default::default()
            }));
        }
    }

    pub(crate) fn find_definition_for_endpoint_literal(
        &self,
        handle: &Handle,
        nodes: &[AnyNodeRef<'_>],
        position: TextSize,
    ) -> Option<Vec1<FindDefinitionItemWithDocstring>> {
        let context = endpoint_call_context(self, handle, nodes, position)?;
        if !context
            .literal
            .value
            .iter()
            .next()?
            .content_range()
            .contains(position)
        {
            return None;
        }
        let request_path = context.literal.value.to_string();
        Vec1::try_from_vec(
            self.collect_endpoint_definitions()
                .into_iter()
                .filter(|endpoint| {
                    method_matches(&endpoint.methods, context.method)
                        && path_matches(&endpoint.path, &request_path)
                })
                .map(|endpoint| FindDefinitionItemWithDocstring {
                    metadata: DefinitionMetadata::VariableOrAttribute(Some(SymbolKind::Function)),
                    module: endpoint.module,
                    definition_range: endpoint.definition_range,
                    docstring_range: endpoint.docstring_range,
                    display_name: None,
                })
                .collect(),
        )
        .ok()
    }
}
