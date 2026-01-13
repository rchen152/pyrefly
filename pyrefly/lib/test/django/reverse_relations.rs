/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::django_testcase;

django_testcase!(
    bug = "Reverse relations not yet implemented",
    test_foreign_key_reverse_default_name,
    r#"
from django.db import models

class Reporter(models.Model):
    full_name = models.CharField(max_length=70)

class Article(models.Model):
    reporter = models.ForeignKey(Reporter, on_delete=models.CASCADE)

reporter = Reporter()
# Default reverse name is <model_lowercase>_set
reporter.article_set  # E: `Reporter` has no attribute `article_set`
"#,
);
