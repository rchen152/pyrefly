/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    test_basic,
    r#"
from typing import Union
X = Union[int, list["X"]]

x1: X = 1
x2: X = [1]
x3: X = [[1]]

x4: X = ["oops"]  # E: not assignable
    "#,
);

testcase!(
    test_display,
    r#"
from typing import reveal_type, TypeAlias, Union

X1 = Union[int, list["X1"]]
X2: TypeAlias = Union[int, list["X2"]]
type X3 = int | list["X3"]

Y1 = Union[int, list["Y2"]]
Y2 = Union[int, list["Y1"]]

def f(x1: X1, x2: X2, x3: X3, y1: Y1, y2: Y2):
    reveal_type(x1)  # E: int | list[X1]
    reveal_type(x2)  # E: int | list[X2]
    reveal_type(x3)  # E: int | list[X3]
    reveal_type(y1)  # E: int | list[Y2]
    reveal_type(y2)  # E: int | list[Y1]
    "#,
);

testcase!(
    test_iterate,
    r#"
type X = int | list[X]
def f(x: X) -> X | None:
    if isinstance(x, list):
        for y in x:
            if y:
                return y
    "#,
);

testcase!(
    test_import,
    TestEnv::one("foo", "type X = int | list[X]"),
    r#"
import foo
x1: foo.X = [[1]]
x2: foo.X = [["oops"]]  # E: not assignable
    "#,
);

testcase!(
    test_from_import,
    TestEnv::one("foo", "type X = int | list[X]"),
    r#"
from foo import X
x1: X = [[1]]
x2: X = [["oops"]]  # E: not assignable
    "#,
);

testcase!(
    test_equivalent,
    r#"
type X = int | list[X]
type Y = int | list[Y]
def f(x: X) -> Y:
    return x
    "#,
);
