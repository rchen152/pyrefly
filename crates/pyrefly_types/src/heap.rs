/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Type heap for allocating types.
//!
//! This module implements a factory pattern for type construction. In the future,
//! this will be backed by a per-module arena for efficient allocation and to enable
//! Copy type references.
//!
//! Currently this is a pass-through factory that returns boxed types, allowing
//! incremental migration of construction sites before switching to arena allocation.

use crate::callable::Callable;
use crate::callable::Function;
use crate::callable::Params;
use crate::class::ClassType;
use crate::keywords::KwCall;
use crate::quantified::Quantified;
use crate::types::BoundMethod;
use crate::types::Forall;
use crate::types::Forallable;
use crate::types::SuperObj;
use crate::types::Type;
use crate::types::Union;

/// A factory for constructing types.
///
/// Currently returns boxed types; will be backed by an arena in the future.
#[derive(Debug, Default)]
pub struct TypeHeap(());

impl TypeHeap {
    /// Create a new type heap.
    pub fn new() -> Self {
        Self(())
    }

    /// Allocate a type in the heap.
    ///
    /// Currently this is a pass-through; in the future it will allocate in the arena.
    pub fn mk(&self, ty: Type) -> Type {
        ty
    }

    /// Create a `Type::None`.
    pub fn mk_none(&self) -> Type {
        Type::None
    }

    /// Create a `Type::Union` from members.
    pub fn mk_union(&self, members: Vec<Type>) -> Type {
        Type::Union(Box::new(Union {
            members,
            display_name: None,
        }))
    }

    /// Create a `Type::Union` with a display name.
    pub fn mk_union_with_name(&self, members: Vec<Type>, display_name: Box<str>) -> Type {
        Type::Union(Box::new(Union {
            members,
            display_name: Some(display_name),
        }))
    }

    /// Create a `Type::Callable` from params and return type.
    pub fn mk_callable(&self, params: Params, ret: Type) -> Type {
        Type::Callable(Box::new(Callable { params, ret }))
    }

    /// Create a `Type::Callable` from an existing Callable.
    pub fn mk_callable_from(&self, callable: Callable) -> Type {
        Type::Callable(Box::new(callable))
    }

    /// Create a `Type::Function` from a Function.
    pub fn mk_function(&self, func: Function) -> Type {
        Type::Function(Box::new(func))
    }

    /// Create a `Type::Type` wrapping an inner type.
    pub fn mk_type(&self, inner: Type) -> Type {
        Type::Type(Box::new(inner))
    }

    /// Create a `Type::Quantified` from a Quantified.
    pub fn mk_quantified(&self, quantified: Quantified) -> Type {
        Type::Quantified(Box::new(quantified))
    }

    /// Create a `Type::Forall` from a Forall.
    pub fn mk_forall(&self, forall: Forall<Forallable>) -> Type {
        Type::Forall(Box::new(forall))
    }

    /// Create a `Type::BoundMethod` from a BoundMethod.
    pub fn mk_bound_method(&self, bound_method: BoundMethod) -> Type {
        Type::BoundMethod(Box::new(bound_method))
    }

    /// Create a `Type::Unpack` wrapping an inner type.
    pub fn mk_unpack(&self, inner: Type) -> Type {
        Type::Unpack(Box::new(inner))
    }

    /// Create a `Type::TypeGuard` wrapping an inner type.
    pub fn mk_type_guard(&self, inner: Type) -> Type {
        Type::TypeGuard(Box::new(inner))
    }

    /// Create a `Type::TypeIs` wrapping an inner type.
    pub fn mk_type_is(&self, inner: Type) -> Type {
        Type::TypeIs(Box::new(inner))
    }

    /// Create a `Type::KwCall` from a KwCall.
    pub fn mk_kw_call(&self, kw_call: KwCall) -> Type {
        Type::KwCall(Box::new(kw_call))
    }

    /// Create a `Type::ElementOfTypeVarTuple` from a Quantified.
    pub fn mk_element_of_type_var_tuple(&self, quantified: Quantified) -> Type {
        Type::ElementOfTypeVarTuple(Box::new(quantified))
    }

    /// Create a `Type::SuperInstance` from lookup class and object.
    pub fn mk_super_instance(&self, lookup_cls: ClassType, obj: SuperObj) -> Type {
        Type::SuperInstance(Box::new((lookup_cls, obj)))
    }

    /// Create a `Type::ArgsValue` from a Quantified.
    pub fn mk_args_value(&self, quantified: Quantified) -> Type {
        Type::ArgsValue(Box::new(quantified))
    }

    /// Create a `Type::KwargsValue` from a Quantified.
    pub fn mk_kwargs_value(&self, quantified: Quantified) -> Type {
        Type::KwargsValue(Box::new(quantified))
    }
}
