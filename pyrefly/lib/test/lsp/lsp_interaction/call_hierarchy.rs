/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_server::Message;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_types::Url;
use lsp_types::notification::DidOpenTextDocument;
use lsp_types::request::CallHierarchyPrepare;
use serde_json::json;

use crate::test::lsp::lsp_interaction::object_model::InitializeSettings;
use crate::test::lsp::lsp_interaction::object_model::LspInteraction;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;

/// Tests that prepareCallHierarchy returns a valid CallHierarchyItem for a function definition
#[test]
fn test_prepare_call_hierarchy_on_function() {
    let root = get_test_files_root();
    let mut interaction = LspInteraction::new();
    interaction.set_root(root.path().to_path_buf());
    interaction
        .initialize(InitializeSettings::default())
        .unwrap();

    let test_file = root.path().join("basic.py");
    let uri = Url::from_file_path(&test_file).unwrap();

    // Open a file with a simple function
    interaction
        .client
        .send_notification::<DidOpenTextDocument>(json!({
            "textDocument": {
                "uri": uri.to_string(),
                "languageId": "python",
                "version": 1,
                "text": "def foo():\n    pass\n",
            }
        }));

    interaction.client.expect_any_message().unwrap();

    // Send a textDocument/prepareCallHierarchy request at the function name
    interaction.client.send_message(Message::Request(Request {
        id: RequestId::from(1),
        method: "textDocument/prepareCallHierarchy".to_owned(),
        params: json!({
            "textDocument": {
                "uri": uri.to_string()
            },
            "position": {
                "line": 0,
                "character": 5
            }
        }),
    }));

    // Expect a successful response with a CallHierarchyItem for the function
    interaction
        .client
        .expect_response::<CallHierarchyPrepare>(
            RequestId::from(1),
            json!([{
                "name": "foo",
                "kind": 12, // SymbolKind::FUNCTION
                "tags": null,
                "detail": "basic.foo",
                "uri": uri.to_string(),
                "range": {
                    "start": {"line": 0, "character": 0},
                    "end": {"line": 1, "character": 8}
                },
                "selectionRange": {
                    "start": {"line": 0, "character": 4},
                    "end": {"line": 0, "character": 7}
                },
                "data": null
            }]),
        )
        .unwrap();

    interaction.shutdown().unwrap();
}

/// Tests that prepareCallHierarchy works when invoked on a call site (not just on the definition)
#[test]
fn test_prepare_call_hierarchy_on_call_site() {
    let root = get_test_files_root();
    let root_path = root.path().join("call_hierarchy_test");
    let scope_uri = Url::from_file_path(&root_path).unwrap();
    let mut interaction = LspInteraction::new();
    interaction.set_root(root_path.clone());
    interaction
        .initialize(InitializeSettings {
            workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
            configuration: Some(None),
            ..Default::default()
        })
        .unwrap();

    let caller_file = root_path.join("caller.py");
    let callee_file = root_path.join("callee.py");

    // Open both files
    interaction.client.did_open("caller.py");
    interaction.client.did_open("callee.py");

    let caller_uri = Url::from_file_path(&caller_file).unwrap();
    let callee_uri = Url::from_file_path(&callee_file).unwrap();

    // Send a textDocument/prepareCallHierarchy request at a call site (my_function() in caller.py)
    // caller.py line 9 (0-indexed) is: "    my_function()"
    interaction.client.send_message(Message::Request(Request {
        id: RequestId::from(1),
        method: "textDocument/prepareCallHierarchy".to_owned(),
        params: json!({
            "textDocument": {
                "uri": caller_uri.to_string()
            },
            "position": {
                "line": 9,  // Line with my_function() call in caller_one
                "character": 6  // On "my_function"
            }
        }),
    }));

    // Should return the CallHierarchyItem for the function definition in callee.py
    interaction
        .client
        .expect_response::<CallHierarchyPrepare>(
            RequestId::from(1),
            json!([{
                "name": "my_function",
                "kind": 12, // SymbolKind::FUNCTION
                "tags": null,
                "detail": "callee.my_function",
                "uri": callee_uri.to_string(),  // Should point to callee.py, not caller.py
                "range": {
                    "start": {"line": 6, "character": 0},
                    "end": {"line": 8, "character": 13}
                },
                "selectionRange": {
                    "start": {"line": 6, "character": 4},
                    "end": {"line": 6, "character": 15}
                },
                "data": null
            }]),
        )
        .unwrap();

    interaction.shutdown().unwrap();
}
