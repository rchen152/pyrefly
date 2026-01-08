/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::attrs_testcase;

attrs_testcase!(
    bug = "Recognize default decorator",
    field_default_decorator,
    r#"
from attrs import define, field

@define
class C:
    a: dict = field()

    @a.default # E: Object of class `dict` has no attribute `default`
    def _default_a(self):
        return {}
"#,
);
