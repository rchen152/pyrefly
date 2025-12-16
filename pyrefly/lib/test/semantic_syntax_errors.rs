/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

// LateFutureImport: `from __future__ import` statements must appear at the beginning of the file,
// after the module docstring and any comments, but before any other code.
testcase!(
    test_late_future_import,
    r#"
import os
from __future__ import annotations  # E: __future__ imports must be at the top of the file
"#,
);

testcase!(
    test_late_future_import_after_assignment,
    r#"
x = 1
from __future__ import annotations  # E: __future__ imports must be at the top of the file
"#,
);

testcase!(
    test_valid_future_import,
    r#"
from __future__ import annotations
import os
x = 1
"#,
);

testcase!(
    test_valid_future_import_after_docstring,
    r#"
"""Module docstring."""
from __future__ import annotations
import os
"#,
);

testcase!(
    test_duplicate_parameter,
    r#"
def foo(x: int, x: str):  # E: Duplicate parameter "x"
    pass
"#,
);
