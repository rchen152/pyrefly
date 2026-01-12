/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::lsp::lsp_interaction::object_model::InitializeSettings;
use crate::test::lsp::lsp_interaction::object_model::LspInteraction;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;

fn check_inlay_hint_label_values(hint: &lsp_types::InlayHint, expected: &[(&str, bool)]) -> bool {
    match &hint.label {
        lsp_types::InlayHintLabel::LabelParts(parts) => {
            if parts.len() != expected.len() {
                return false;
            }
            for (part, (expected_value, should_have_location)) in parts.iter().zip(expected.iter())
            {
                if part.value != *expected_value {
                    return false;
                }
                if *should_have_location && part.location.is_none() {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

#[test]
fn test_inlay_hints() {
    let root = get_test_files_root();
    let mut interaction = LspInteraction::new();
    interaction.set_root(root.path().to_path_buf());
    interaction
        .initialize(InitializeSettings {
            configuration: Some(None),
            ..Default::default()
        })
        .unwrap();
    interaction.open_notebook(
        "notebook.ipynb",
        vec![
            "def no_return_annot():\n    _ = (1, 2)  # no inlay hint here\n    return (1, 2)",
            "result = no_return_annot()",
            "async def foo():\n    return 0",
        ],
    );

    interaction
        .inlay_hint_cell("notebook.ipynb", "cell1", 0, 0, 100, 0)
        .expect_response_with(|result| {
            let hints = match result {
                Some(hints) => hints,
                None => return false,
            };
            if hints.len() != 1 {
                return false;
            }
            let hint = &hints[0];
            if hint.position.line != 0 || hint.position.character != 21 {
                return false;
            }
            check_inlay_hint_label_values(
                hint,
                &[
                    (" -> ", false),
                    ("tuple", false),
                    ("[", false),
                    ("Literal", false),
                    ("[", false),
                    ("1", false),
                    ("]", false),
                    (", ", false),
                    ("Literal", false),
                    ("[", false),
                    ("2", false),
                    ("]", false),
                    ("]", false),
                ],
            )
        })
        .unwrap();

    interaction
        .inlay_hint_cell("notebook.ipynb", "cell2", 0, 0, 100, 0)
        .expect_response_with(|result| {
            let hints = match result {
                Some(hints) => hints,
                None => return false,
            };
            if hints.len() != 1 {
                return false;
            }
            let hint = &hints[0];
            if hint.position.line != 0 || hint.position.character != 6 {
                return false;
            }
            check_inlay_hint_label_values(
                hint,
                &[
                    (": ", false),
                    ("tuple", false),
                    ("[", false),
                    ("Literal", false),
                    ("[", false),
                    ("1", false),
                    ("]", false),
                    (", ", false),
                    ("Literal", false),
                    ("[", false),
                    ("2", false),
                    ("]", false),
                    ("]", false),
                ],
            )
        })
        .unwrap();

    interaction
        .inlay_hint_cell("notebook.ipynb", "cell3", 0, 0, 100, 0)
        .expect_response_with(|result| {
            let hints = match result {
                Some(hints) => hints,
                None => return false,
            };
            if hints.len() != 1 {
                return false;
            }
            let hint = &hints[0];
            if hint.position.line != 0 || hint.position.character != 15 {
                return false;
            }
            check_inlay_hint_label_values(
                hint,
                &[
                    (" -> ", false),
                    ("Literal", false),
                    ("[", false),
                    ("0", false),
                    ("]", false),
                ],
            )
        })
        .unwrap();

    interaction.shutdown().unwrap();
}
