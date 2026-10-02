# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Type stubs for torch.distributions.constraints."""

from typing import Any

from shape_extensions import IntTuple, IntVar
from torch import Tensor

class Constraint:
    """Base class for constraints."""

    is_discrete: bool = False
    event_dim: int = 0

    def check(self, value: Tensor) -> Tensor: ...

class _Boolean(Constraint):
    is_discrete: bool = True

    def check[S: IntTuple](self, value: Tensor[S]) -> Tensor[S]: ...

class _OneHot(Constraint):
    is_discrete: bool = True
    event_dim: int = 1

    def check[S: IntTuple, N: IntVar](self, value: Tensor[[*S, N]]) -> Tensor[S]: ...

class _Simplex(Constraint):
    event_dim: int = 1

    def check[S: IntTuple, N: IntVar](self, value: Tensor[[*S, N]]) -> Tensor[S]: ...

class _Square(Constraint):
    event_dim: int = 2

    def check[S: IntTuple, M: IntVar, N: IntVar](
        self, value: Tensor[[*S, M, N]]
    ) -> Tensor[S]: ...

class _Symmetric(_Square): ...

real: Constraint
boolean: _Boolean
one_hot: _OneHot
simplex: _Simplex
square: _Square
symmetric: _Symmetric

def interval(lower_bound: float, upper_bound: float) -> Constraint: ...

# TODO: Replace these availability stubs with precise declarations.
MixtureSameFamilyConstraint: Any
cat: Any
corr_cholesky: Any
dependent: Any
dependent_property: Any
greater_than: Any
greater_than_eq: Any
half_open_interval: Any
independent: Any
integer_interval: Any
is_dependent: Any
less_than: Any
lower_cholesky: Any
lower_triangular: Any
multinomial: Any
nonnegative: Any
nonnegative_integer: Any
positive: Any
positive_definite: Any
positive_integer: Any
positive_semidefinite: Any
real_vector: Any
stack: Any
unit_interval: Any
