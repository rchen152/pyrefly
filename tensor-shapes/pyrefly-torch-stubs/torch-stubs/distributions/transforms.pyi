# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Type stubs for torch.distributions.transforms."""

from typing import Any

from shape_extensions import broadcast, IntTuple, IntVar
from torch import Tensor
from torch.distributions import Distribution

class Transform:
    """Base class for invertible transforms with computable log det Jacobians."""

    domain: Any
    codomain: Any
    bijective: bool
    sign: int

    def __init__(self, cache_size: int = 0) -> None: ...
    def __call__[S: IntTuple](self, x: Tensor[S]) -> Tensor[S]: ...
    def _call[S: IntTuple](self, x: Tensor[S]) -> Tensor[S]: ...
    def _inverse[S: IntTuple](self, y: Tensor[S]) -> Tensor[S]: ...
    def log_abs_det_jacobian(self, x: Tensor, y: Tensor) -> Tensor: ...

class AbsTransform(Transform): ...

class ExpTransform(Transform):
    def log_abs_det_jacobian[S: IntTuple](
        self, x: Tensor[S], y: Tensor[S]
    ) -> Tensor[S]: ...

class SigmoidTransform(Transform):
    def log_abs_det_jacobian[S: IntTuple](
        self, x: Tensor[S], y: Tensor[S]
    ) -> Tensor[S]: ...

class SoftplusTransform(Transform):
    def log_abs_det_jacobian[S: IntTuple](
        self, x: Tensor[S], y: Tensor[S]
    ) -> Tensor[S]: ...

class TanhTransform(Transform):
    def log_abs_det_jacobian[S: IntTuple](
        self, x: Tensor[S], y: Tensor[S]
    ) -> Tensor[S]: ...

class LowerCholeskyTransform(Transform): ...
class PositiveDefiniteTransform(Transform): ...
class SoftmaxTransform(Transform): ...

class StickBreakingTransform(Transform):
    def __call__[S: IntTuple, N: IntVar](
        self, x: Tensor[[*S, N]]
    ) -> Tensor[[*S, N + 1]]: ...
    def log_abs_det_jacobian[S: IntTuple, N: IntVar](
        self, x: Tensor[[*S, N]], y: Tensor[[*S, N + 1]]
    ) -> Tensor[S]: ...

class AffineTransform[Loc: IntTuple = [], Scale: IntTuple = []](Transform):
    def __init__(
        self,
        loc: Tensor[Loc] | float,
        scale: Tensor[Scale] | float,
        event_dim: int = 0,
        cache_size: int = 0,
    ) -> None: ...
    def __call__[S: IntTuple](
        self, x: Tensor[S]
    ) -> Tensor[broadcast(broadcast(S, Loc), Scale)]: ...

class PowerTransform[Exponent: IntTuple](Transform):
    def __init__(self, exponent: Tensor[Exponent], cache_size: int = 0) -> None: ...
    def __call__[S: IntTuple](self, x: Tensor[S]) -> Tensor[broadcast(S, Exponent)]: ...

class CumulativeDistributionTransform[DistShape: IntTuple](Transform):
    def __init__(
        self, distribution: Distribution[DistShape], cache_size: int = 0
    ) -> None: ...
    def __call__[S: IntTuple](
        self, x: Tensor[S]
    ) -> Tensor[broadcast(S, DistShape)]: ...

# TODO: Replace these availability stubs with shape-aware declarations.
CatTransform: Any
ComposeTransform: Any
CorrCholeskyTransform: Any
IndependentTransform: Any
ReshapeTransform: Any
StackTransform: Any
identity_transform: Any
