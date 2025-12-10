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
use serde_json::json;

use crate::test::lsp::lsp_interaction::object_model::InitializeSettings;
use crate::test::lsp::lsp_interaction::object_model::LspInteraction;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;

/// todo(jvansch): Update this test once prepareCallHierarchy is implemented
#[test]
fn test_prepare_call_hierarchy_not_implemented() {
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

    // Try to send a textDocument/prepareCallHierarchy request
    // This should fail with "Unknown request" since it's not yet hooked up
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

    interaction
        .client
        .expect_response_error(
            RequestId::from(1),
            json!({
                "code": -32601,
                "message": "Unknown request: textDocument/prepareCallHierarchy",
                "data": null,
            }),
        )
        .unwrap();

    interaction.shutdown().unwrap();
}
