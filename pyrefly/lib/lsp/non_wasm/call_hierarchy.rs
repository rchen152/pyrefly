/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::module::Module;
use pyrefly_python::module::TextRangeWithModule;
use pyrefly_python::sys_info::SysInfo;
use pyrefly_util::task_heap::Cancelled;
use pyrefly_util::visit::Visit;
use ruff_python_ast::Expr;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::state::lsp::DefinitionMetadata;
use crate::state::state::CancellableTransaction;

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
                            Self::find_containing_function_for_call(
                                handle,
                                &ast,
                                call.range().start(),
                            )
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
