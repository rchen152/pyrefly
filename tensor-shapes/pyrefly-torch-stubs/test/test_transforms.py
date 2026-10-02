# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from typing import assert_type, TYPE_CHECKING

import torch
from shape_extensions import assert_shape, IntVar
from torch import Tensor
from torch.distributions.transforms import (
    AbsTransform,
    ExpTransform,
    SigmoidTransform,
    SoftplusTransform,
    TanhTransform,
)


def test_elementwise_transform_shapes() -> None:
    x = torch.randn(2, 3)

    assert_shape(AbsTransform()(x).shape, (2, 3))
    for transform in (
        ExpTransform(),
        SigmoidTransform(),
        SoftplusTransform(),
        TanhTransform(),
    ):
        y = transform(x)
        assert_shape(y.shape, (2, 3))
        assert_shape(transform.log_abs_det_jacobian(x, y).shape, (2, 3))


if TYPE_CHECKING:

    def check_symbolic_elementwise_transform_shapes[N: IntVar, M: IntVar](
        x: Tensor[[N, M]],
    ) -> None:
        assert_type(AbsTransform()(x), Tensor[[N, M]])
        for transform in (
            ExpTransform(),
            SigmoidTransform(),
            SoftplusTransform(),
            TanhTransform(),
        ):
            y = transform(x)
            assert_type(y, Tensor[[N, M]])
            assert_type(transform.log_abs_det_jacobian(x, y), Tensor[[N, M]])
