# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from typing import assert_type, TYPE_CHECKING

import torch
from shape_extensions import assert_shape, IntTuple, IntVar
from torch import Tensor
from torch.distributions import constraints


def test_elementwise_constraint() -> None:
    value = torch.tensor([[0.0, 1.0, 2.0], [1.0, 0.0, 1.0]])

    assert_shape(constraints.boolean.check(value).shape, (2, 3))
    assert constraints.boolean.check(value).tolist() == [
        [True, True, False],
        [True, True, True],
    ]


def test_vector_constraints() -> None:
    value = torch.tensor([[1.0, 0.0, 0.0], [0.2, 0.3, 0.5]])

    assert_shape(constraints.one_hot.check(value).shape, (2,))
    assert constraints.one_hot.check(value).tolist() == [True, False]
    assert_shape(constraints.simplex.check(value).shape, (2,))
    assert constraints.simplex.check(value).tolist() == [True, True]


def test_matrix_constraints() -> None:
    square = torch.tensor([[[1.0, 2.0], [2.0, 3.0]]])
    rectangular = torch.ones((3, 2))

    assert_shape(constraints.square.check(square).shape, (1,))
    assert constraints.square.check(square).tolist() == [True]
    assert_shape(constraints.symmetric.check(square).shape, (1,))
    assert constraints.symmetric.check(square).tolist() == [True]
    assert_shape(constraints.square.check(rectangular).shape, ())
    assert not constraints.square.check(rectangular).item()
    assert not constraints.symmetric.check(rectangular).item()


if TYPE_CHECKING:

    def check_constraint_shapes[S: IntTuple, M: IntVar, N: IntVar](
        elementwise: Tensor[S],
        vector: Tensor[[*S, N]],
        matrix: Tensor[[*S, M, N]],
    ) -> None:
        assert_type(constraints.boolean.check(elementwise), Tensor[S])
        assert_type(constraints.one_hot.check(vector), Tensor[S])
        assert_type(constraints.simplex.check(vector), Tensor[S])
        assert_type(constraints.square.check(matrix), Tensor[S])
        assert_type(constraints.symmetric.check(matrix), Tensor[S])
