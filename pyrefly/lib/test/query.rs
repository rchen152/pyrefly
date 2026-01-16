/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests for the query interface, specifically get_types_in_file.

use pretty_assertions::assert_eq;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use pyrefly_util::arc_id::ArcId;
use pyrefly_util::fs_anyhow;
use serde_json::Value;
use serde_json::json;
use tempfile::TempDir;

use crate::config::config::ConfigFile;
use crate::config::finder::ConfigFinder;
use crate::query::Query;
use crate::test::util::init_test;

/// Helper to create a Query with a ConfigFinder that doesn't use sourcedb.
fn create_query() -> Query {
    init_test();
    let mut config = ConfigFile::default();
    config.python_environment.set_empty_to_default();
    config.configure();
    let config = ArcId::new(config);
    Query::new(ConfigFinder::new_constant(config))
}

/// Convert the result of get_types_in_file to JSON for testing.
fn types_to_json(types: Vec<(crate::query::PythonASTRange, String)>) -> Value {
    let entries: Vec<Value> = types
        .into_iter()
        .map(|(range, type_str)| {
            json!({
                "location": {
                    "start_line": range.start_line.get(),
                    "start_col": range.start_col,
                    "end_line": range.end_line.get(),
                    "end_col": range.end_col
                },
                "type": type_str
            })
        })
        .collect();
    json!(entries)
}

#[test]
fn test_simple_int_annotation() {
    let tdir = TempDir::new().unwrap();
    let file_path = tdir.path().join("main.py");
    let code = "x: int = 42";
    fs_anyhow::write(&file_path, code).unwrap();

    let query = create_query();
    let module_name = ModuleName::from_str("main");
    let path = ModulePath::filesystem(file_path.clone());

    // Load the file
    let errors = query.add_files(vec![(module_name, path.clone())]);
    assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);

    // Get types
    let types = query.get_types_in_file(module_name, path).unwrap();
    let json = types_to_json(types);

    // Expected: the variable 'x', the annotation 'int', and the literal '42'
    let expected = json!([
        {
            "location": {
                "start_line": 1,
                "start_col": 0,
                "end_line": 1,
                "end_col": 1
            },
            "type": "builtins.int"
        },
        {
            "location": {
                "start_line": 1,
                "start_col": 3,
                "end_line": 1,
                "end_col": 6
            },
            "type": "type[builtins.int]"
        },
        {
            "location": {
                "start_line": 1,
                "start_col": 9,
                "end_line": 1,
                "end_col": 11
            },
            "type": "typing.Literal[42]"
        }
    ]);

    assert_eq!(json, expected);
}
