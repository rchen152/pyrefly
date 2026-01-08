/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::CallHierarchyIncomingCall;
use lsp_types::CallHierarchyItem;
use lsp_types::SymbolKind;
use pyrefly_build::handle::Handle;
use pyrefly_python::ast::Ast;
use pyrefly_python::module::Module;
use pyrefly_python::module::TextRangeWithModule;
use pyrefly_python::sys_info::SysInfo;
use pyrefly_util::task_heap::Cancelled;
use pyrefly_util::visit::Visit;
use ruff_python_ast::AnyNodeRef;
use ruff_python_ast::Expr;
use ruff_python_ast::ModModule;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::lsp::non_wasm::module_helpers::module_info_to_uri;
use crate::state::lsp::DefinitionMetadata;
use crate::state::state::CancellableTransaction;

/// Creates call hierarchy information for the function containing a call site.
///
/// Given a call site position, finds the enclosing function and returns
/// information needed to represent it in the call hierarchy.
pub fn find_containing_function_for_call(
    handle: &Handle,
    ast: &ModModule,
    position: TextSize,
) -> Option<(String, TextRange)> {
    let covering_nodes = Ast::locate_node(ast, position);

    // Look through the node chain for the containing function
    for (i, node) in covering_nodes.iter().enumerate() {
        if let AnyNodeRef::StmtFunctionDef(func_def) = node {
            // Check if this is a method (next node is a ClassDef)
            if let Some(AnyNodeRef::StmtClassDef(class_def)) = covering_nodes.get(i + 1) {
                let name = format!(
                    "{}.{}.{}",
                    handle.module(),
                    class_def.name.id,
                    func_def.name.id
                );
                return Some((name, func_def.name.range()));
            } else {
                // Top-level function
                let name = format!("{}.{}", handle.module(), func_def.name.id);
                return Some((name, func_def.name.range()));
            }
        }
    }
    None
}

/// Converts raw incoming call data to LSP CallHierarchyIncomingCall items.
///
/// Takes the output from `find_global_incoming_calls_from_function_definition`
/// and transforms it into the LSP response format.
pub fn transform_incoming_calls(
    callers: Vec<(Module, Vec<(TextRange, String, TextRange)>)>,
) -> Vec<CallHierarchyIncomingCall> {
    let mut incoming_calls = Vec::new();
    for (caller_module, call_sites) in callers {
        for (call_range, caller_name, caller_def_range) in call_sites {
            let Some(caller_uri) = module_info_to_uri(&caller_module) else {
                continue;
            };

            let from = CallHierarchyItem {
                name: caller_name
                    .split('.')
                    .next_back()
                    .unwrap_or(&caller_name)
                    .to_owned(),
                kind: SymbolKind::FUNCTION,
                tags: None,
                detail: Some(caller_name),
                uri: caller_uri,
                range: caller_module.to_lsp_range(caller_def_range),
                selection_range: caller_module.to_lsp_range(caller_def_range),
                data: None,
            };

            incoming_calls.push(CallHierarchyIncomingCall {
                from,
                from_ranges: vec![caller_module.to_lsp_range(call_range)],
            });
        }
    }
    incoming_calls
}

impl CancellableTransaction<'_> {
    /// Finds all incoming calls (functions that call this function) of a function across the entire codebase.
    ///
    /// This searches transitive reverse dependencies to find all locations where
    /// the target function is called. For each call site, it identifies the
    /// containing function.
    ///
    /// Returns a vector of tuples containing:
    /// - Module where the call occurs
    /// - Vector of (call_site_range, containing_function_name, containing_function_range)
    ///
    /// Returns Err if the request is canceled during execution.
    pub fn find_global_incoming_calls_from_function_definition(
        &mut self,
        sys_info: &SysInfo,
        definition_kind: DefinitionMetadata,
        target_definition: &TextRangeWithModule,
    ) -> Result<Vec<(Module, Vec<(TextRange, String, TextRange)>)>, Cancelled> {
        // Use process_rdeps_with_definition to find references and filter to call sites in a single pass
        let results = self.process_rdeps_with_definition(
            sys_info,
            target_definition,
            |transaction, handle, patched_definition| {
                let module_info = transaction.as_ref().get_module_info(handle)?;
                let ast = transaction.as_ref().get_ast(handle)?;

                let references = transaction
                    .as_ref()
                    .local_references_from_definition(
                        handle,
                        definition_kind.clone(),
                        patched_definition.range,
                        &patched_definition.module,
                    )
                    .unwrap_or_default();

                if references.is_empty() {
                    return None;
                }

                let ref_set: std::collections::HashSet<TextRange> =
                    references.into_iter().collect();

                let mut callers_in_file = Vec::new();

                ast.visit(&mut |expr| {
                    if let Expr::Call(call) = expr
                        && ref_set
                            .iter()
                            .any(|ref_range| call.func.range().contains(ref_range.start()))
                        && let Some((containing_func_name, containing_func_range)) =
                            find_containing_function_for_call(handle, &ast, call.range().start())
                    {
                        callers_in_file.push((
                            call.range(),
                            containing_func_name,
                            containing_func_range,
                        ));
                    }
                });

                if callers_in_file.is_empty() {
                    None
                } else {
                    Some((module_info, callers_in_file))
                }
            },
        )?;

        Ok(results)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use pyrefly_build::handle::Handle;
    use pyrefly_python::ast::Ast;
    use pyrefly_python::module_name::ModuleName;
    use pyrefly_python::module_path::ModulePath;
    use pyrefly_python::sys_info::SysInfo;
    use ruff_python_ast::PySourceType;
    use ruff_text_size::TextSize;

    use super::find_containing_function_for_call;

    #[test]
    fn test_find_containing_function_for_call() {
        let source = r#"
def my_function():
    x = call()

class MyClass:
    def method(self):
        y = call()
"#;
        let (ast, _, _) = Ast::parse(source, PySourceType::Python);
        let handle = Handle::new(
            ModuleName::from_str("test"),
            ModulePath::memory(PathBuf::from("test.py")),
            SysInfo::default(),
        );

        // Returns qualified name for top-level function
        let pos_in_func = TextSize::from(30);
        let (name, _) = find_containing_function_for_call(&handle, &ast, pos_in_func).unwrap();
        assert_eq!(name, "test.my_function");

        // Returns qualified name for class method
        let pos_in_method = TextSize::from(85);
        let (name, _) = find_containing_function_for_call(&handle, &ast, pos_in_method).unwrap();
        assert_eq!(name, "test.MyClass.method");
    }
}
