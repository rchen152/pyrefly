# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from typing import assert_type, cast, TYPE_CHECKING

import torch
from shape_extensions import assert_shape, IntTuple
from torch import return_types, Tensor


def test_cholesky_ex_named_result() -> None:
    result = cast(
        "return_types.linalg_cholesky_ex[[2, 3, 3], [2]]",
        torch.linalg.cholesky_ex(torch.eye(3).expand((2, 3, 3))),
    )
    factor, info = result
    assert_shape(factor.shape, (2, 3, 3))
    assert_shape(info.shape, (2,))
    assert_shape(result.L.shape, (2, 3, 3))
    assert_shape(result.info.shape, (2,))


def test_inv_ex_named_result() -> None:
    result = cast(
        "return_types.linalg_inv_ex[[2, 3, 3], [2]]",
        torch.linalg.inv_ex(torch.eye(3).expand((2, 3, 3))),
    )
    inverse, info = result
    assert_shape(inverse.shape, (2, 3, 3))
    assert_shape(info.shape, (2,))
    assert_shape(result.inverse.shape, (2, 3, 3))
    assert_shape(result.info.shape, (2,))


def test_solve_ex_named_result() -> None:
    result = cast(
        "return_types.linalg_solve_ex[[2, 3, 4], [2]]",
        torch.linalg.solve_ex(torch.eye(3).expand((2, 3, 3)), torch.ones((2, 3, 4))),
    )
    solution, info = result
    assert_shape(solution.shape, (2, 3, 4))
    assert_shape(info.shape, (2,))
    assert_shape(result.result.shape, (2, 3, 4))
    assert_shape(result.info.shape, (2,))


if TYPE_CHECKING:

    def check_ex_named_results[Matrix: IntTuple, Batch: IntTuple, RHS: IntTuple](
        chol: return_types.linalg_cholesky_ex[Matrix, Batch],
        inverse: return_types.linalg_inv_ex[Matrix, Batch],
        solution: return_types.linalg_solve_ex[RHS, Batch],
    ) -> None:
        assert_type(chol[0], Tensor[Matrix])
        assert_type(chol[1], Tensor[Batch])
        assert_type(chol.L, Tensor[Matrix])
        assert_type(chol.info, Tensor[Batch])
        assert_type(inverse[0], Tensor[Matrix])
        assert_type(inverse[1], Tensor[Batch])
        assert_type(inverse.inverse, Tensor[Matrix])
        assert_type(inverse.info, Tensor[Batch])
        assert_type(solution[0], Tensor[RHS])
        assert_type(solution[1], Tensor[Batch])
        assert_type(solution.result, Tensor[RHS])
        assert_type(solution.info, Tensor[Batch])
