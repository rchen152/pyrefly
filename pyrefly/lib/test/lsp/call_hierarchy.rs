/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::Itertools as _;
use pretty_assertions::assert_eq;
use pyrefly_build::handle::Handle;
use pyrefly_python::module::TextRangeWithModule;
use ruff_text_size::TextSize;

use crate::state::lsp::FindPreference;
use crate::state::state::State;
use crate::test::util::code_frame_of_source_at_range;
use crate::test::util::get_batched_lsp_operations_report;

fn get_callers_report(state: &State, handle: &Handle, position: TextSize) -> String {
    let mut transaction = state.cancellable_transaction();

    let Some(def_item) = transaction
        .as_ref()
        .find_definition(handle, position, FindPreference::default())
        .into_iter()
        .next()
    else {
        return "Callers Result: None".to_owned();
    };

    let definition = TextRangeWithModule::new(def_item.module.clone(), def_item.definition_range);

    let callers = match transaction.find_global_incoming_calls_from_function_definition(
        handle.sys_info(),
        def_item.metadata.clone(),
        &definition,
    ) {
        Ok(callers) => callers,
        Err(_) => return "Callers Result: Cancelled".to_owned(),
    };

    if !callers.is_empty() {
        callers
            .into_iter()
            .flat_map(|(module_info, calls)| {
                calls
                    .into_iter()
                    .map(move |(call_range, caller_name, _caller_range)| {
                        format!(
                            "Caller: {}\nCall site:\n{}",
                            caller_name,
                            code_frame_of_source_at_range(module_info.contents(), call_range),
                        )
                    })
            })
            .join("\n")
    } else {
        "Callers Result: None".to_owned()
    }
}

#[test]
fn find_callers_simple_test() {
    let code = r#"
def greet(name: str) -> str:
#   ^
    return f"Hello, {name}!"

def test_greet():
    result = greet("World")
    print(result)

def another_caller():
    msg = greet("Alice")
    return msg
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.test_greet"));
    assert!(report.contains("Caller: main.another_caller"));
    assert!(report.contains("greet(\"World\")"));
    assert!(report.contains("greet(\"Alice\")"));
}

#[test]
fn find_callers_no_calls_test() {
    let code = r#"
def unused_function():
#   ^
    return "I'm never called"

def some_other_function():
    return "I do my own thing"
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert_eq!(
        r#"
# main.py
2 | def unused_function():
        ^
Callers Result: None
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn find_callers_nested_calls_test() {
    let code = r#"
def helper() -> int:
#   ^
    return 42

def level1():
    return helper()

def level2():
    value = helper()
    return value * 2

def level3():
    a = helper()
    b = helper()
    return a + b
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.level1"));
    assert!(report.contains("Caller: main.level2"));
    assert!(report.contains("Caller: main.level3"));
    // level3 should have two call sites to helper()
    let level3_count = report.matches("Caller: main.level3").count();
    assert_eq!(level3_count, 2, "level3 should have two calls to helper()");
}

#[test]
fn find_callers_nested_in_other_statements_works_at_definition_test() {
    let code = r#"
def helper() -> int:
#   ^
    return 42

def level1():
    if True:
        helper()
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.level1"));
}

#[test]
fn find_callers_nested_in_other_statements_works_at_call_site_definition_test() {
    let code = r#"
def helper() -> int:
    return 42

def level1():
    if True:
        helper()
#       ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.level1"));
}

#[test]
fn find_callers_method_at_definition_test() {
    let code = r#"
class A:
    def greet(self):
#       ^
        print("hello world")

def call_class():
    a = A()
    a.greet()
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.call_class"));
}

#[test]
fn find_callers_method_at_call_site_test() {
    let code = r#"
class A:
    def greet(self):
        print("hello world")

def call_class():
    a = A()
    a.greet()
#     ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_callers_report);
    assert!(report.contains("# main.py"));
    assert!(report.contains("Caller: main.call_class"));
}
