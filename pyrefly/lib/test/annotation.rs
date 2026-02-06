/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_python::sys_info::PythonVersion;

use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    bug = "conformance: Type quotes incorrectly referring to shadowing class member. Note that this test is likely to be removed: https://github.com/python/typing/pull/2144",
    test_class_scope_quoted_annotation_bypasses_shadowing,
    r#"
from typing import assert_type
class D:
    def int(self) -> None:
        ...
    x: "int" = 0  # E: Expected a type form
assert_type(D.x, int)  # E: assert_type(Any, int) failed
"#,
);

testcase!(
    test_union_operator_with_bare_string_literal,
    TestEnv::new_with_version(PythonVersion::new(3, 13, 0)),
    r#"
from typing import assert_type, TypeVar, Generic
T = TypeVar("T")
class C(Generic[T]): ...
bad1: int | "str" = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
bad2: int | "str" | T = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
bad3: "str" | int = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
bad4: "str" | int | T = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
bad5: C | "str" = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
ok1: T | "str" = "foo"
ok2: "str" | T = "foo"
ok3 = list["str" | T]
ok4 = (int) | (str)
ok5: "str" | C[int] = "foo"
ok6: C[int] | "str" = "foo"
"#,
);

testcase!(
    bug = "These should be errors",
    test_union_of_plain_type_and_complex_forward_ref,
    r#"
# More complex types in quotes - current implementation only detects simple names
# so "list[str]" (which parses to a subscript, not a name) is not detected
bad1: int | "list[str]" = []
bad2: "list[str]" | int = []
    "#,
);

testcase!(
    bug = "This should be an error because `|` is not supported between strings",
    test_union_of_forward_refs,
    r#"
bad: "int" | "list[str]" = 1
    "#,
);

// Test that the error is NOT raised for Python 3.14+
// In Python 3.14+, annotations are not evaluated at runtime by default (PEP 649)
testcase!(
    test_union_type_with_bare_string_literal_py314,
    TestEnv::new_with_version(PythonVersion::new(3, 14, 0)),
    r#"
ok1: int | "str" = "foo"
ok2: "str" | int = "foo"
"#,
);

// Test that the error is NOT raised when `from __future__ import annotations` is used
// With future annotations, annotations are not evaluated at runtime
testcase!(
    test_union_type_with_bare_string_literal_future_annotations,
    TestEnv::new_with_version(PythonVersion::new(3, 13, 0)),
    r#"
from __future__ import annotations
ok1: int | "str" = "foo"
ok2: "str" | int = "foo"
"#,
);

// Test legacy type alias with forward reference string literal
testcase!(
    bug = "Missing error on `bad2`",
    test_union_operator_with_legacy_type_alias,
    TestEnv::new_with_version(PythonVersion::new(3, 13, 0)),
    r#"
from typing import TypeAlias

class C[T]: pass

IntAlias1 = C[int]
IntAlias2: TypeAlias = C[int]
IntAlias3 = int
IntAlias4: TypeAlias = int

ok1: IntAlias1 | "str" = "foo"
ok2: IntAlias2 | "str" = "foo"
bad1: IntAlias3 | "str" = "foo"  # E: Cannot use `|` operator with forward reference string literal and type
bad2: IntAlias4 | "str" = "foo"
"#,
);

// Test scoped type alias with forward reference string literal
// TypeAliasType is a plain type, so this is a runtime error
testcase!(
    bug = "We should error because `|` does not work with TypeAliasType",
    test_union_operator_with_scoped_type_alias,
    TestEnv::new_with_version(PythonVersion::new(3, 13, 0)),
    r#"
class C[T]: pass
type IntAlias = C[int]
bad: IntAlias | "str" = "foo"
"#,
);
