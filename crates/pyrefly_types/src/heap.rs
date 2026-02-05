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
use crate::callable::Params;
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

    /// Create a `Type::Type` wrapping an inner type.
    pub fn mk_type(&self, inner: Type) -> Type {
        Type::Type(Box::new(inner))
    }
}
