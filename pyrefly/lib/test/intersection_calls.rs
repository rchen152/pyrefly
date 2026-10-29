/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    bug = "Intersection calls lose nested gradual return guarantees",
    test_intersection_call_nested_gradual_results,
    r#"
from typing import Any, assert_type
class A:
    def method(self) -> list[Any]: ...
class B:
    def method(self) -> list[int]: ...
class C:
    def method(self) -> list[int]: ...
class D:
    def method(self) -> list[Any]: ...
def f(a: A, c: C):
    if isinstance(a, B):
        assert_type(a.method(), Any)
    if isinstance(c, D):
        assert_type(c.method(), Any)
"#,
);

testcase!(
    bug = "Intersection calls lose complementary gradual return guarantees",
    test_intersection_call_complementary_gradual_results,
    r#"
from typing import Any, assert_type
class A:
    def method(self) -> dict[Any, int]: ...
class B:
    def method(self) -> dict[str, Any]: ...
class C:
    def method(self) -> tuple[Any, int]: ...
class D:
    def method(self) -> tuple[str, Any]: ...
def f(a: A, c: C):
    if isinstance(a, B):
        result = a.method()
        assert_type(result["key"], Any)
    if isinstance(c, D):
        result = c.method()
        assert_type(result[1], Any)
"#,
);

// Resolving a callable fallback also exposes its deprecation metadata.
testcase!(
    test_intersection_call_single_callable_deprecation,
    r#"
from warnings import deprecated

class Marker: ...
class CallableA:
    @deprecated("use a different callable")
    def __call__(self, x: int) -> int: ...

def invoke(value: Marker):
    if isinstance(value, CallableA):
        value(1)  # E: `CallableA.__call__` is deprecated
"#,
);

testcase!(
    bug = "Intersection calls report only the fallback's deprecation",
    test_intersection_call_multiple_callable_deprecations,
    r#"
from warnings import deprecated

class A:
    @deprecated("use a different A")
    def __call__(self, x: int) -> int: ...
class B:
    @deprecated("use a different B")
    def __call__(self, x: int) -> int: ...

def invoke(value: A):
    if isinstance(value, B):
        value(1)  # E: `B.__call__` is deprecated
"#,
);
