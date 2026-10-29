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

// A typed call through an intersection must retain the declared union-valued fields.
testcase!(
    test_intersection_call_type_any_return_preserves_union_fields,
    r#"
from typing import Any, assert_type

class StrawberryType: ...
class StrawberryUnion(StrawberryType):
    def __init__(self, name: str | None = None) -> None:
        self.graphql_name = name
class StrawberryList(StrawberryType):
    def __init__(self, of_type: StrawberryType | type) -> None:
        self.of_type = of_type
class LazyType:
    def resolve_type(self) -> type[Any]: ...
def eval_type(value: Any) -> type: ...

def names(value: StrawberryType | type):
    value = eval_type(value)
    if isinstance(value, LazyType):
        value = value.resolve_type()
    if isinstance(value, StrawberryUnion):
        assert_type(value.graphql_name, str | None)
    elif isinstance(value, StrawberryList):
        assert_type(value.of_type, StrawberryType | type[Any])
"#,
);

testcase!(
    bug = "Gradual intersection fields discard known union annotations",
    test_intersection_type_any_union_field_fold,
    r#"
from typing import Any, assert_type

class StrawberryType: ...
class StrawberryUnion(StrawberryType):
    def __init__(self, name: str | None = None) -> None:
        self.graphql_name = name
class StrawberryList(StrawberryType):
    def __init__(self, of_type: StrawberryType | type) -> None:
        self.of_type = of_type

def names(value: type[Any]):
    if isinstance(value, StrawberryUnion):
        assert_type(value.graphql_name, Any)
    elif isinstance(value, StrawberryList):
        assert_type(value.of_type, Any)
"#,
);

testcase!(
    bug = "Intersection calls lose shared-method deprecation metadata",
    test_intersection_call_three_callable_deprecations,
    r#"
from warnings import deprecated

class A:
    @deprecated("A method")
    def method(self) -> int: ...
class B:
    @deprecated("B method")
    def method(self) -> int: ...
class C:
    @deprecated("C method")
    def method(self) -> int: ...

def invoke(value: A):
    if isinstance(value, B) and isinstance(value, C):
        value.method()
"#,
);

// Callable signature compatibility must not discard a mock instance's attributes.
testcase!(
    test_intersection_mock_field_preserves_attributes,
    r#"
from typing import assert_type
from unittest.mock import Mock

class SignatureOwner:
    def close(self) -> None: ...
class MockOwner:
    def __init__(self) -> None:
        self.close = Mock()

def signature_first(value: SignatureOwner):
    if isinstance(value, MockOwner):
        assert_type(value.close.called, bool)
        value.close.assert_called()
def nominal_first(value: MockOwner):
    if isinstance(value, SignatureOwner):
        assert_type(value.close.called, bool)
        value.close.assert_called()
"#,
);

testcase!(
    test_intersection_callable_instance_preserves_marker,
    r#"
from typing import assert_type

class Callback:
    marker: int
    def __call__(self) -> None: ...
class SignatureOwner:
    def callback(self) -> None: ...
class NominalOwner:
    def __init__(self) -> None:
        self.callback = Callback()

def signature_first(value: SignatureOwner):
    if isinstance(value, NominalOwner):
        assert_type(value.callback.marker, int)
        value.callback()
def nominal_first(value: NominalOwner):
    if isinstance(value, SignatureOwner):
        assert_type(value.callback.marker, int)
        value.callback()
"#,
);

// A real gradual return must not be confused with the representation's fallback.
testcase!(
    bug = "Intersection calls erase concrete guarantees alongside real Any returns",
    test_intersection_call_real_gradual_return,
    r#"
from typing import Any, reveal_type

class Result:
    value: int
class Precise:
    def make(self) -> Result: ...
class Gradual:
    def make(self) -> Any: ...

def precise_first(value: Precise):
    if isinstance(value, Gradual):
        result = value.make()
        reveal_type(result)  # E: revealed type: Unknown
        reveal_type(result.value)  # E: revealed type: Unknown
        result.dynamic_only

def gradual_first(value: Gradual):
    if isinstance(value, Precise):
        result = value.make()
        reveal_type(result)  # E: revealed type: Unknown
        reveal_type(result.value)  # E: revealed type: Unknown
        result.dynamic_only
"#,
);

testcase!(
    test_intersection_call_fallback_does_not_add_gradual_return,
    r#"
from typing import assert_type

class Result:
    value: int
class Precise:
    def make(self) -> Result: ...
class Marker: ...

def invoke(value: Precise):
    if isinstance(value, Marker):
        result = value.make()
        assert_type(result, Result)
        assert_type(result.value, int)
        result.dynamic_only  # E: Object of class `Result` has no attribute `dynamic_only`
"#,
);

testcase!(
    bug = "Intersection calls erase the concrete part of gradual final returns",
    test_intersection_call_gradual_final_return,
    r#"
from typing import Any, final, reveal_type

@final
class FinalResult: ...
class Precise:
    def make(self) -> FinalResult: ...
class Gradual:
    def make(self) -> Any: ...

def invoke(value: Precise):
    if isinstance(value, Gradual):
        result = value.make()
        reveal_type(result)  # E: revealed type: Unknown
        result.missing
"#,
);
