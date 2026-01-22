/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use pyrefly_types::literal::Lit;
use pyrefly_types::types::Union;

use crate::state::state::Transaction;
use crate::types::types::Type;

impl Transaction<'_> {
    /// Adds completion items for literal types (e.g., `Literal["foo", "bar"]`).
    pub(crate) fn add_literal_completions_from_type(
        param_type: &Type,
        completions: &mut Vec<CompletionItem>,
        in_string_literal: bool,
    ) {
        match param_type {
            Type::Literal(lit) => {
                // TODO: Pass the flag correctly for whether literal string is single quoted or double quoted
                let label = lit.value.to_string_escaped(true);
                let insert_text = if in_string_literal {
                    if let Lit::Str(s) = &lit.value {
                        s.to_string()
                    } else {
                        label.clone()
                    }
                } else {
                    label.clone()
                };
                completions.push(CompletionItem {
                    label,
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("{param_type}")),
                    insert_text: Some(insert_text),
                    ..Default::default()
                });
            }
            Type::Union(box Union { members, .. }) => {
                for member in members {
                    Self::add_literal_completions_from_type(member, completions, in_string_literal);
                }
            }
            _ => {}
        }
    }
}
