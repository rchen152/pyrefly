/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

// LateFutureImport: `from __future__ import` statements must appear at the beginning of the file,
// after the module docstring and any comments, but before any other code.
testcase!(
    test_late_future_import,
    r#"
import os
from __future__ import annotations  # E: __future__ imports must be at the top of the file
"#,
);

testcase!(
    test_late_future_import_after_assignment,
    r#"
x = 1
from __future__ import annotations  # E: __future__ imports must be at the top of the file
"#,
);

testcase!(
    test_valid_future_import,
    r#"
from __future__ import annotations
import os
x = 1
"#,
);

testcase!(
    test_valid_future_import_after_docstring,
    r#"
"""Module docstring."""
from __future__ import annotations
import os
"#,
);

testcase!(
    test_duplicate_parameter,
    r#"
def foo(x: int, x: str):  # E: Duplicate parameter "x"
    pass
"#,
);

testcase!(
    test_rebound_comprehension_variable,
    r#"
[y := 1 for y in range(10)]  # E: assignment expression cannot rebind comprehension variable
"#,
);

testcase!(
    test_nonlocal_at_module_level,
    r#"
nonlocal x  # E: nonlocal declaration not allowed at module level
"#,
);

testcase!(
    test_nonlocal_in_class_body,
    r#"
x = 1
class Foo:
    nonlocal x  # E: Found `x`, but it is coming from the global scope
"#,
);

testcase!(
    test_nonlocal_inside_function_ok,
    r#"
def outer():
    x = 1
    def inner():
        nonlocal x
        x = 2
"#,
);

testcase!(
    test_multiple_case_assignment,
    r#"
x = [1, 2]
match x:
    case [a, a]:  # E: multiple assignments to name `a` in pattern
        pass
"#,
);

testcase!(
    test_duplicate_match_key,
    r#"
x = {"a": 1, "b": 2}
match x:
    case {"a": 1, "a": 2}:  # E: mapping pattern checks duplicate key `"a"`
        pass
"#,
);
