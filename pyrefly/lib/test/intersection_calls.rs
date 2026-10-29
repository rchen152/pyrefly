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

// These scenarios cross-check constituent-wise calls against ty's intersection tests.
testcase!(
    bug = "Intersection calls discard the successful constituents' return contracts",
    test_intersection_call_three_way_partial_success,
    r#"
from typing import reveal_type

class RetA:
    left: int
class RetB:
    right: str
class AcceptA:
    def __call__(self) -> RetA: ...
class AcceptB:
    def __call__(self) -> RetB: ...
class Reject:
    def __call__(self, required: int) -> int: ...

def invoke(value: AcceptA):
    if isinstance(value, AcceptB) and isinstance(value, Reject):
        result = value()  # E: Missing argument `required` in function `Reject.__call__`
        reveal_type(result)  # E: revealed type: int
        reveal_type(result.left)  # E: revealed type: Unknown  # E: Object of class `int` has no attribute `left`
        reveal_type(result.right)  # E: revealed type: Unknown  # E: Object of class `int` has no attribute `right`
"#,
);

testcase!(
    bug = "A union preserves the fallback instead of intersection return guarantees",
    test_union_of_intersections_call,
    r#"
from typing import reveal_type

class RetA: ...
class RetB: ...
class RetC: ...
class IntA:
    def __call__(self, x: int) -> RetA: ...
class IntB:
    def __call__(self, x: int) -> RetB: ...
class IntC:
    def __call__(self, x: int) -> RetC: ...
class StrC:
    def __call__(self, x: str) -> RetC: ...

def accepts(value: IntA, other: IntC, choose: bool):
    if isinstance(value, IntB):
        selected = value if choose else other
        reveal_type(selected(1))  # E: revealed type: RetB | RetC

def rejects(value: IntA, other: StrC, choose: bool):
    if isinstance(value, IntB):
        selected = value if choose else other
        selected(1)  # E: Argument `Literal[1]` is not assignable to parameter `x` with type `str`
"#,
);

testcase!(
    bug = "Wrapped intersection constructors use only their fallback class",
    test_intersection_class_constructors,
    r#"
from typing import reveal_type

class A:
    left: int
class B:
    right: str

def construct(cls: type[A]):
    if issubclass(cls, B):
        result = cls()
        reveal_type(cls)  # E: revealed type: type[A & B]
        reveal_type(result)  # E: revealed type: B
        reveal_type(result.left)  # E: revealed type: Unknown  # E: Object of class `B` has no attribute `left`
        reveal_type(result.right)  # E: revealed type: str
"#,
);

// A generic class identity cannot authorize arguments rejected by its narrowed constructor.
testcase!(
    test_intersection_constructor_typevar_does_not_bypass_arguments,
    r#"
from typing import TypeVar, assert_type

class A:
    def __init__(self, value: int) -> None: ...
class B:
    def __init__(self, value: int) -> None: ...
T = TypeVar("T", A, B)

def constrained(cls: type[T]) -> T:
    if issubclass(cls, A):
        return cls("wrong")  # E: Argument `Literal['wrong']` is not assignable  # E: Returned type `A` is not assignable to declared return type `T`
    return cls(1)

def constrained_positive(cls: type[T]) -> T:
    return cls(1)
assert_type(constrained_positive(A), A)
assert_type(constrained_positive(B), B)

class Base:
    def __init__(self) -> None: ...
class Child(Base):
    def __init__(self, required: int) -> None: ...
U = TypeVar("U", bound=Base)

def bounded(cls: type[U]) -> U:
    if issubclass(cls, Child):
        return cls()  # E: Missing argument `required`  # E: Returned type `Child` is not assignable to declared return type `U`
    return cls()

def bounded_positive(cls: type[U]) -> U:
    return cls()
assert_type(bounded_positive(Base), Base)
"#,
);

testcase!(
    bug = "Intersection calls discard disjoint return contracts",
    test_intersection_call_gradual_return_order,
    r#"
from typing import Any, reveal_type
class A:
    def make(self) -> int: ...
class B:
    def make(self) -> Any: ...
class C:
    def make(self) -> str: ...
class D:
    def make(self) -> int: ...
class E:
    def make(self) -> str: ...
class F:
    def make(self) -> Any: ...
def gradual_middle(x: A):
    if isinstance(x, B) and isinstance(x, C):
        reveal_type(x.make())  # E: revealed type: Unknown
def gradual_last(x: D):
    if isinstance(x, E) and isinstance(x, F):
        reveal_type(x.make())  # E: revealed type: Unknown
"#,
);

testcase!(
    bug = "Calls lose return guarantees for overlapping dictionary views",
    test_intersection_call_typed_dict_and_dict_overlap,
    r#"
from typing import TypedDict, reveal_type
class TD(TypedDict):
    key: int
class A:
    def make(self) -> TD: ...
    field: TD
class B:
    def make(self) -> dict[str, int]: ...
    field: dict[str, int]
class C:
    def make(self) -> dict[str, int]: ...
    field: dict[str, int]
class D:
    def make(self) -> TD: ...
    field: TD

def td_first(x: A):
    if isinstance(x, B):
        reveal_type(x.make())  # E: revealed type: Unknown
        reveal_type(x.field)  # E: revealed type: dict[str, int] & TD
def dict_first(x: C):
    if isinstance(x, D):
        reveal_type(x.make())  # E: revealed type: Unknown
        reveal_type(x.field)  # E: revealed type: dict[str, int] & TD

witness: TD = {"key": 1}
assert isinstance(witness, dict) and witness["key"] == 1
"#,
);

testcase!(
    bug = "Ellipsis callables skip keyword expression diagnostics",
    test_ellipsis_keyword_expression_errors,
    r#"
from typing import Callable
def invoke(callback: Callable[..., int]):
    callback(x=[(1).missing])
    callback(**{"x": (1).missing})
    callback(x=[unknown])  # E: Could not find name `unknown`
    callback([(1).missing])  # E: Object of class `int` has no attribute `missing`
"#,
);

testcase!(
    test_intersection_call_intrinsic_constructor_and_keyword_errors,
    r#"
from typing import Callable
class A:
    def __init__(self, xs: list[int]) -> None: ...
class B:
    def __init__(self, xs: list[int]) -> None: ...
def constructors(cls: type[A]):
    if issubclass(cls, B):
        cls([(1).missing])  # E: Object of class `int` has no attribute `missing`

class RetA: ...
class RetB: ...
class C:
    callback: Callable[..., RetA]
class D:
    callback: Callable[[int], RetB]
def keyword_ellipsis(x: C):
    if isinstance(x, D):
        x.callback(x=[(1).missing])  # E: Object of class `int` has no attribute `missing`
"#,
);

testcase!(
    test_intersection_constructor_foreign_new_result,
    r#"
from typing import TypeVar, assert_type
class A:
    marker: str
    def __new__(cls, x: int) -> int:
        return x
class B:
    def __new__(cls, x: int) -> int:
        return x
T = TypeVar("T", A, B)
def constraints(cls: type[T]):
    if issubclass(cls, A):
        result = cls(1)
        assert_type(result, int)
        result.marker  # E: Object of class `int` has no attribute `marker`
class Base:
    def __new__(cls) -> int:
        return 1
class Child(Base):
    marker: str
U = TypeVar("U", bound=Base)
def bounded(cls: type[U]):
    if issubclass(cls, Child):
        result = cls()
        assert_type(result, int)
        result.marker  # E: Object of class `int` has no attribute `marker`
assert_type(A(1), int)
assert_type(Child(), int)
"#,
);

testcase!(
    bug = "Fallback calls hide first-use inference for deferred container arguments",
    test_intersection_call_deferred_partial_state,
    r#"
from typing import reveal_type
class A:
    def method(self, x: list[list[int]], tag: str) -> int: ...
class B:
    def method(self, x: list[list[str]], tag: int) -> str: ...
class C:
    def method(self, x: list[list[str]], tag: int) -> str: ...
class D:
    def method(self, x: list[list[int]], tag: str) -> int: ...

def failed_int_first(a: A):
    if isinstance(a, B):
        empty = []
        result = a.method([empty], 1)
        reveal_type(result)  # E: revealed type: Unknown
        empty.append("ok")
        reveal_type(empty)  # E: revealed type: list[Unknown]
def successful_str_first(c: C):
    if isinstance(c, D):
        empty = []
        result = c.method([empty], 1)
        reveal_type(result)  # E: revealed type: Unknown
        empty.append("ok")
        reveal_type(empty)  # E: revealed type: list[Unknown]
"#,
);

testcase!(
    bug = "Fallback hides indexing of intersection attribute contracts",
    test_intersection_attribute_indexing_contracts,
    r#"
from typing import reveal_type
class Marker: ...
class A:
    data: list[int]
class B:
    data: Marker
class C:
    data: Marker
class D:
    data: list[int]
def list_first(a: A):
    if isinstance(a, B):
        reveal_type(a.data)  # E: revealed type: Marker & list[int]
        reveal_type(a.data[0])  # E: revealed type: Unknown
        reveal_type(a.data["bad"])  # E: revealed type: Unknown
def marker_first(c: C):
    if isinstance(c, D):
        reveal_type(c.data)  # E: revealed type: Marker & list[int]
        reveal_type(c.data[0])  # E: revealed type: Unknown
"#,
);

testcase!(
    bug = "Fallback hides contextual comprehension inference in intersection calls",
    test_intersection_call_contextual_comprehensions,
    r#"
from typing import assert_type, overload, reveal_type
class A:
    def method(self, xs: list[float]) -> int: ...
class B:
    def method(self, xs: list[float]) -> int: ...
class C:
    @overload
    def method(self, xs: list[float]) -> int: ...
    @overload
    def method(self, xs: str) -> str: ...
    def method(self, xs) -> int | str: ...
class D:
    @overload
    def method(self, xs: list[float]) -> int: ...
    @overload
    def method(self, xs: str) -> str: ...
    def method(self, xs) -> int | str: ...
def first(a: A):
    if isinstance(a, B):
        reveal_type(a.method([i for i in range(3)]))  # E: revealed type: Unknown
        reveal_type(a.method(xs=[i for i in range(3)]))  # E: revealed type: Unknown
def reverse(b: B):
    if isinstance(b, A):
        reveal_type(b.method([i for i in range(3)]))  # E: revealed type: Unknown
def overloaded(c: C):
    if isinstance(c, D):
        reveal_type(c.method([i for i in range(3)]))  # E: revealed type: Unknown
def ordinary(a: A):
    assert_type(a.method([i for i in range(3)]), int)
"#,
);

testcase!(
    bug = "Fallback loses results and argument context for intersected callbacks",
    test_intersection_call_representative_lambda_context,
    r#"
from typing import Callable, assert_type, reveal_type
class RetA: ...
class RetB: ...
class A:
    def method(self, callback: Callable[[int], int]) -> RetA: ...
class B:
    def method(self, callback: Callable[[str], int]) -> RetB: ...
class C:
    def method(self, callback: Callable[[str], int]) -> RetB: ...
class D:
    def method(self, callback: Callable[[int], int]) -> RetA: ...
def int_first(a: A):
    if isinstance(a, B):
        reveal_type(a.method(lambda v: v + 1))  # E: revealed type: Unknown
        typed: Callable[[int], int] = lambda v: v + 1
        reveal_type(a.method(typed))  # E: revealed type: Unknown
def str_first(c: C):
    if isinstance(c, D):
        reveal_type(c.method(lambda v: v + 1))  # E: revealed type: Unknown
def ordinary(a: A):
    assert_type(a.method(lambda v: v + 1), RetA)
def intrinsic_errors(a: A):
    if isinstance(a, B):
        a.method(lambda v: unknown)  # E: Could not find name `unknown`
        a.method(lambda v: (1).missing)  # E: Object of class `int` has no attribute `missing`
"#,
);

testcase!(
    bug = "Narrowed generic Self constructors lose identity and argument checks",
    test_intersection_constructor_generic_self_required_argument,
    r#"
from typing import Generic, Self, TypeVar, assert_type
class Parent[T]:
    def __init__(self) -> None: ...
    @classmethod
    def make(cls) -> Self:
        if issubclass(cls, Child):
            return cls()  # E: Missing argument `required`  # E: Returned type
        return cls()
class Child(Parent[int]):
    def __init__(self, required: int) -> None: ...
T = TypeVar("T")
class LegacyParent(Generic[T]):
    def __init__(self) -> None: ...
    @classmethod
    def make(cls) -> Self:
        if issubclass(cls, LegacyChild):
            return cls()  # E: Missing argument `required`  # E: Returned type
        return cls()
class LegacyChild(LegacyParent[int]):
    def __init__(self, required: int) -> None: ...
assert_type(Child(1), Child)
assert_type(LegacyChild(1), LegacyChild)
"#,
);

testcase!(
    bug = "Fallback conflates selected subclass and independent constructor views",
    test_intersection_constructor_unrestricted_and_mixin_boundaries,
    r#"
from typing import TypeVar, assert_type
U = TypeVar("U")
class A:
    def __init__(self, required: int) -> None: ...
def unrestricted(cls: type[U]) -> U:
    if issubclass(cls, A):
        return cls()  # E: Missing argument `required`  # E: Returned type
    return cls()
class Base:
    def __init__(self) -> None: ...
class Mixin:
    def __init__(self, required: int) -> None: ...
V = TypeVar("V", bound=Base)
def unrelated_mixin(cls: type[V]) -> V:
    if issubclass(cls, Mixin):
        return cls()  # E: Missing argument `required`  # E: Returned type
    return cls()
class Combined(Base, Mixin): ...
assert_type(Combined(), Combined)
assert_type(A(1), A)
"#,
);

testcase!(
    bug = "Fallback loses static fields around a real gradual return contract",
    test_intersection_call_gradual_three_way_fields,
    r#"
from typing import Any, reveal_type
class RetA:
    a: int
class RetC:
    c: str
class A:
    def make(self) -> RetA: ...
class B:
    def make(self) -> Any: ...
class C:
    def make(self) -> RetC: ...
class D:
    def make(self) -> RetA: ...
class E:
    def make(self) -> RetC: ...
class F:
    def make(self) -> Any: ...
def gradual_middle(a: A):
    if isinstance(a, B) and isinstance(a, C):
        result = a.make()
        reveal_type(result.a)  # E: revealed type: Unknown
        reveal_type(result.c)  # E: revealed type: Unknown
def gradual_last(d: D):
    if isinstance(d, E) and isinstance(d, F):
        result = d.make()
        reveal_type(result.a)  # E: revealed type: Unknown
        reveal_type(result.c)  # E: revealed type: Unknown
"#,
);

testcase!(
    bug = "Narrowed protocol-bound constructors lose generic result identity",
    test_intersection_constructor_structural_protocol_bound,
    r#"
from typing import Protocol, TypeVar, reveal_type
class P(Protocol):
    def method(self) -> None: ...
class Child:
    def __init__(self, required: int) -> None: ...
    def method(self) -> None: ...
T = TypeVar("T", bound=P)
def bad(cls: type[T]) -> T:
    if issubclass(cls, Child):
        return cls()  # E: Missing argument `required`  # E: Returned type
    return cls()
def good(cls: type[T]) -> T:
    if issubclass(cls, Child):
        result = cls(1)
        reveal_type(result)  # E: revealed type: Child
        return result  # E: Returned type
    return cls()
class GenericP[S](Protocol):
    def value(self) -> S: ...
class IntChild:
    def __init__(self, required: int) -> None: ...
    def value(self) -> int: ...
def generic_good[U: GenericP[int]](cls: type[U]) -> U:
    if issubclass(cls, IntChild):
        return cls(1)  # E: Returned type
    return cls()
"#,
);

testcase!(
    bug = "Fallback loses call guarantees from three overlapping dictionaries",
    test_intersection_call_three_dictionary_guarantees,
    r#"
from typing import TypedDict, reveal_type
class Left(TypedDict):
    left: int
class Right(TypedDict):
    right: str
class Flag(TypedDict):
    flag: bool
class A:
    field: Left
    def make(self) -> Left: ...
class B:
    field: Right
    def make(self) -> Right: ...
class C:
    field: Flag
    def make(self) -> Flag: ...
class D:
    field: dict[str, object]
    def make(self) -> dict[str, object]: ...
def typed_dicts(a: A):
    if isinstance(a, B) and isinstance(a, C):
        reveal_type(a.make())  # E: revealed type: Unknown
        reveal_type(a.field)  # E: revealed type: Flag & Left & Right
def dictionary(d: D):
    if isinstance(d, A) and isinstance(d, B):
        reveal_type(d.make())  # E: revealed type: Unknown
        reveal_type(d.field)  # E: revealed type: dict[str, object] & Left & Right
witness = {"left": 1, "right": "ok", "flag": True}
assert witness["left"] == 1 and witness["right"] == "ok" and witness["flag"] is True
"#,
);

testcase!(
    test_intersection_comprehension_scope_binding_order,
    r#"
from typing import overload
class A:
    def method(self, x: object) -> None: ...
class B:
    def method(self, x: object) -> None: ...
@overload
def g(x: list[float]) -> float: ...
@overload
def g(x: str) -> str: ...
def g(x) -> float | str: ...
def use_later(a: A):
    if isinstance(a, B):
        a.method(later)
later = g([i for i in range(3)])  # E: No matching overload
earlier = g([i for i in range(3)])  # E: No matching overload
def use_earlier(a: A):
    if isinstance(a, B):
        a.method(earlier)
"#,
);

testcase!(
    bug = "Fallback loses specialized subscription information in intersections",
    test_intersection_specialized_subscription_views,
    r#"
from typing import TypedDict, reveal_type
class Left(TypedDict):
    left: int
class Right(TypedDict):
    right: str
class A:
    data: Left
class B:
    data: Right
class Marker: ...
class C:
    data: tuple[int, str]
class D:
    data: Marker
def dictionaries(a: A):
    if isinstance(a, B):
        reveal_type(a.data["left"])  # E: revealed type: Unknown
        reveal_type(a.data["right"])  # E: revealed type: Unknown
        reveal_type(a.data["absent"])  # E: revealed type: Unknown
def tuples(c: C):
    if isinstance(c, D):
        reveal_type(c.data[0])  # E: revealed type: Unknown
        reveal_type(c.data[1])  # E: revealed type: Unknown
        reveal_type(c.data[8])  # E: revealed type: Unknown
"#,
);

testcase!(
    bug = "Fallback loses overlapping TypedDict and Mapping call guarantees",
    test_intersection_call_typed_dict_mapping_overlap,
    r#"
from typing import Mapping, NotRequired, TypedDict, reveal_type
class TD(TypedDict):
    key: int
    optional: NotRequired[str]
class A:
    field: TD
    def make(self) -> TD: ...
class B:
    field: Mapping[str, int]
    def make(self) -> Mapping[str, int]: ...
def overlap(a: A):
    if isinstance(a, B):
        reveal_type(a.make())  # E: revealed type: Unknown
        reveal_type(a.field)  # E: revealed type: Mapping[str, int] & TD
witness: TD = {"key": 1}
assert witness["key"] == 1 and "optional" not in witness
"#,
);

testcase!(
    test_intersection_custom_slice_validation,
    r#"
from typing import reveal_type
class Custom:
    def __getitem__(self, key: slice) -> list[int]:
        return []
def list_first(xs: list[int]):
    if isinstance(xs, Custom):
        reveal_type(xs[::0])  # E: revealed type: list[int]
        reveal_type(xs["start":"stop"])  # E: revealed type: list[int]
def custom_first(xs: Custom):
    if isinstance(xs, list):
        xs[::0]  # E: step
        xs["start":"stop"]  # E: Slice indices must be integers

def builtin_only(xs: list[int]):
    xs[::0]  # E: step
"#,
);

testcase!(
    bug = "Fallback hides bounded contextual inference for nested comprehensions",
    test_intersection_nested_comprehension_deferral,
    r#"
from typing import reveal_type
class A:
    def method(self, x: list[float]) -> float: ...
class B:
    def method(self, x: list[float]) -> float: ...
def nested(a: A):
    if isinstance(a, B):
        result = a.method([a.method([a.method([1]) for _ in (0,)]) for _ in (0,)])
        reveal_type(result)  # E: revealed type: Unknown
"#,
);
