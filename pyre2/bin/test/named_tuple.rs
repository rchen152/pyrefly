/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_named_tuple,
    r#"
from typing import NamedTuple, assert_type
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
p = Pair(x=1, y="")
x, y = p
assert_type(x, int)
assert_type(y, str)
    "#,
);

testcase!(
    test_named_tuple_subtype,
    r#"
from typing import NamedTuple
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
def func1(x: tuple[int | str, ...]) -> None: ...
def func2(x: tuple[int, str]) -> None: ...
func1(p)
func2(p)
    "#,
);

testcase!(
    test_named_tuple_match,
    r#"
from typing import NamedTuple, assert_type
class Pair(NamedTuple):
    x: int
    y: int
def test(p: Pair):
    match p:
        case Pair(x, y):
            assert_type(x, int)
            assert_type(y, int)
    match p:
        case x, y:
            assert_type(x, int)
            assert_type(y, int)
    "#,
);

testcase!(
    test_named_tuple_iter,
    r#"
from typing import NamedTuple, reveal_type
class Pair(NamedTuple):
    x: int
    y: str

class Pair2[T](NamedTuple):
    x: int
    y: T
    
def test(p: Pair, p2: Pair2[bytes]):
    reveal_type(p.__iter__)  # E: BoundMethod[Pair, (self: Pair) -> Iterable[int | str]]
    reveal_type(p2.__iter__)  # E: BoundMethod[Pair2[bytes], (self: Pair2[bytes]) -> Iterable[bytes | int]]
    "#,
);

testcase_with_bug!(
    "NamedTuple extends tuple[Any, ...], making it a subtype of too many things",
    test_named_tuple_subclass,
    r#"
from typing import NamedTuple, Sequence, Never
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
x1: Sequence[int|str] = p # should succeed
x2: Sequence[Never] = p # should fail
    "#,
);

testcase!(
    test_named_tuple_multiple_inheritance,
    r#"
from typing import NamedTuple
class Foo: pass
class Pair(NamedTuple, Foo):  # E: Named tuples do not support multiple inheritance
    x: int
    y: int
class Pair2(NamedTuple):
    x: int
    y: int
class Pair3(Pair2, Foo):  # E: Named tuples do not support multiple inheritance
    pass
    "#,
);
