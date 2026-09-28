/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;

use anyhow::Context;
use anyhow::Result;
use pyrefly_util::absolutize::Absolutize;
use pyrefly_util::prelude::SliceExt;
use ruff_text_size::Ranged;
use similar::Algorithm;
use similar::DiffOp;
use similar::capture_diff_slices;

use crate::config::config::BaselineMatchingMode;
use crate::error::error::Error;
use crate::error::legacy::BaselineError;
use crate::error::legacy::BaselineErrors;

const INVALID_BASELINE_GUIDANCE: &str =
    "baseline file is invalid; rerun with `--update-baseline` to regenerate it";

/// Keys use absolute paths internally so comparison is independent of the baseline's path format.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BaselineKey {
    path: String,
    name: String,
    matching_field: BaselineMatchingField,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BaselineMatchingField {
    Column(usize),
    ConciseDescription(String),
}

/// Normalize a path to an absolute, forward-slash string.
pub(crate) fn normalize_baseline_path(path: &Path, relative_to: &Path) -> String {
    path.absolutize_from(relative_to)
        .to_string_lossy()
        .replace('\\', "/")
}

impl BaselineKey {
    fn from_baseline_error(
        error: &BaselineError,
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
        entry_index: usize,
    ) -> Result<Self> {
        let matching_field = match matching_mode {
            // `column-ordered` shares the `column` key; the modes differ only in whether
            // the baseline's row order takes part in matching.
            BaselineMatchingMode::Column | BaselineMatchingMode::ColumnOrdered => {
                let mode = if matching_mode.is_ordered() {
                    "column-ordered"
                } else {
                    "column"
                };
                BaselineMatchingField::Column(error.column.with_context(|| {
                    format!(
                        "baseline entry {} (path `{}`, error kind `{}`) is missing field \
                         `column`, required by \
                         `baseline-matching-mode = \"{mode}\"`",
                        entry_index + 1,
                        error.path,
                        error.name,
                    )
                })?)
            }
            BaselineMatchingMode::ConciseDescription => BaselineMatchingField::ConciseDescription(
                error.concise_description.clone().with_context(|| {
                    format!(
                        "baseline entry {} (path `{}`, error kind `{}`) is missing field \
                         `concise_description`, required by \
                         `baseline-matching-mode = \"concise-description\"`",
                        entry_index + 1,
                        error.path,
                        error.name,
                    )
                })?,
            ),
        };
        Ok(Self {
            path: normalize_baseline_path(Path::new(&error.path), relative_to),
            name: error.name.clone(),
            matching_field,
        })
    }

    fn from_error(error: &Error, matching_mode: BaselineMatchingMode) -> Self {
        let matching_field = match matching_mode {
            BaselineMatchingMode::Column | BaselineMatchingMode::ColumnOrdered => {
                BaselineMatchingField::Column(error.display_range().start.column().get() as usize)
            }
            BaselineMatchingMode::ConciseDescription => {
                BaselineMatchingField::ConciseDescription(error.msg_header().to_owned())
            }
        };
        Self {
            path: error.path().as_path().to_string_lossy().replace('\\', "/"),
            name: error.error_kind().to_name().to_owned(),
            matching_field,
        }
    }
}

/// Order diagnostics the way a baseline stores its rows. `--update-baseline` writes rows
/// in this order and `column-ordered` matching sorts diagnostics into it, so the two
/// sequences handed to `align` line up; they must never be sorted two different ways.
pub(crate) fn sort_by_source_position(errors: &mut [Error]) {
    errors.sort_by_cached_key(|error| {
        (
            error.path().to_string(),
            error.range().start(),
            error.range().end(),
            error.error_kind(),
        )
    });
}

/// Align a file's baseline rows against its observed diagnostics, returning which
/// entries of each sequence found a counterpart.
///
/// Both sequences are in source order, and neither key carries a line number, so an
/// unrelated diagnostic appearing earlier in the file shifts nothing: the surrounding
/// keys still align and only the new diagnostic is left over. That is the reason for
/// aligning rather than comparing per-key totals, which would blame the *last*
/// occurrence of a key instead of the one that was added.
///
/// Whether a diagnostic is reported decides whether a check passes, so this uses the
/// plain algorithm rather than a deadline-bounded one: the same inputs have to give the
/// same answer on every machine.
fn align(baseline: &[BaselineKey], observed: &[BaselineKey]) -> (Vec<bool>, Vec<bool>) {
    let mut baseline_matched = vec![false; baseline.len()];
    let mut observed_matched = vec![false; observed.len()];
    for op in capture_diff_slices(Algorithm::Myers, baseline, observed) {
        if let DiffOp::Equal {
            old_index,
            new_index,
            len,
        } = op
        {
            baseline_matched[old_index..old_index + len].fill(true);
            observed_matched[new_index..new_index + len].fill(true);
        }
    }
    (baseline_matched, observed_matched)
}

/// Group positions in a source-ordered key sequence by path, preserving order within
/// each path so that each group can be aligned against that file's baseline rows.
fn group_by_path(keys: &[BaselineKey]) -> Vec<(String, Vec<usize>)> {
    let mut groups: Vec<(String, Vec<usize>)> = Vec::new();
    for (index, key) in keys.iter().enumerate() {
        match groups.last_mut() {
            Some((path, positions)) if *path == key.path => positions.push(index),
            _ => groups.push((key.path.clone(), vec![index])),
        }
    }
    groups
}

/// How rows are looked up, which follows from the matching mode.
#[derive(Debug)]
enum Lookup {
    /// The set-based modes ask only whether a key is present, so one row suppresses any
    /// number of diagnostics that match it.
    Membership(HashSet<BaselineKey>),
    /// `column-ordered` aligns per file, so the baseline's row order within a file is
    /// significant and must not be re-sorted. Maps each path to its row positions.
    Sequences(HashMap<String, Vec<usize>>),
}

/// A parsed baseline: one key per row, indexed for matching.
#[derive(Debug)]
struct BaselineIndex {
    /// The key of each row, in file order.
    keys: Vec<BaselineKey>,
    lookup: Lookup,
    matching_mode: BaselineMatchingMode,
}

impl BaselineIndex {
    fn new(
        rows: &[BaselineError],
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
    ) -> Result<Self> {
        let keys = rows
            .iter()
            .enumerate()
            .map(|(index, row)| {
                BaselineKey::from_baseline_error(row, relative_to, matching_mode, index)
            })
            .collect::<Result<Vec<_>>>()?;
        let lookup = if matching_mode.is_ordered() {
            let mut rows_by_path: HashMap<String, Vec<usize>> = HashMap::new();
            for (position, key) in keys.iter().enumerate() {
                rows_by_path
                    .entry(key.path.clone())
                    .or_default()
                    .push(position);
            }
            Lookup::Sequences(rows_by_path)
        } else {
            Lookup::Membership(keys.iter().cloned().collect())
        };
        Ok(Self {
            keys,
            lookup,
            matching_mode,
        })
    }

    /// Move every diagnostic the baseline covers from `shown_errors` to
    /// `baseline_errors`, and return whether each row covered at least one diagnostic.
    fn apply(&self, shown_errors: &mut Vec<Error>, baseline_errors: &mut Vec<Error>) -> Vec<bool> {
        let (covered, rows_matched) = match &self.lookup {
            Lookup::Membership(lookup) => {
                let observed =
                    shown_errors.map(|error| BaselineKey::from_error(error, self.matching_mode));
                let covered = observed.map(|key| lookup.contains(key));
                let matched_keys: HashSet<&BaselineKey> = observed
                    .iter()
                    .zip(&covered)
                    .filter_map(|(key, covered)| covered.then_some(key))
                    .collect();
                let rows_matched = self.keys.map(|key| matched_keys.contains(key));
                (covered, rows_matched)
            }
            Lookup::Sequences(rows_by_path) => {
                sort_by_source_position(shown_errors);
                let observed =
                    shown_errors.map(|error| BaselineKey::from_error(error, self.matching_mode));
                let mut covered = vec![false; observed.len()];
                let mut rows_matched = vec![false; self.keys.len()];
                for (path, positions) in group_by_path(&observed) {
                    let empty = Vec::new();
                    let row_positions = rows_by_path.get(&path).unwrap_or(&empty);
                    let (rows_used, observed_used) = align(
                        &row_positions.map(|position| self.keys[*position].clone()),
                        &positions.map(|index| observed[*index].clone()),
                    );
                    for (position, used) in row_positions.iter().zip(rows_used) {
                        rows_matched[*position] = used;
                    }
                    for (index, used) in positions.iter().zip(observed_used) {
                        covered[*index] = used;
                    }
                }
                (covered, rows_matched)
            }
        };

        let mut remaining_errors = Vec::new();
        for (error, covered) in shown_errors.drain(..).zip(covered) {
            if covered {
                baseline_errors.push(error);
            } else {
                remaining_errors.push(error);
            }
        }
        *shown_errors = remaining_errors;
        rows_matched
    }
}

/// A lightweight, keys-only baseline matcher for the language server.
#[derive(Debug)]
pub struct BaselineProcessor {
    index: BaselineIndex,
}

impl BaselineProcessor {
    /// Parse the contents of a baseline file. `relative_to` is the base directory
    /// that was used when the baseline was written (i.e. the resolved
    /// `--relative-to` value), so that relative paths in the file are resolved
    /// correctly.
    pub fn from_json(
        content: &str,
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
    ) -> Result<Self> {
        let baseline_file: BaselineErrors =
            serde_json::from_str(content).context(INVALID_BASELINE_GUIDANCE)?;
        Self::from_baseline_errors(baseline_file, relative_to, matching_mode)
            .context(INVALID_BASELINE_GUIDANCE)
    }

    fn from_baseline_errors(
        baseline_errors: BaselineErrors,
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
    ) -> Result<Self> {
        Ok(Self {
            index: BaselineIndex::new(&baseline_errors.errors, relative_to, matching_mode)?,
        })
    }

    /// Baseline suppressions are processed last, after inline and config suppressions.
    ///
    /// Under `column-ordered`, each call must include every diagnostic for the files it
    /// covers, because each file's rows are aligned against that file's diagnostics.
    pub fn process_errors(&self, shown_errors: &mut Vec<Error>, baseline_errors: &mut Vec<Error>) {
        self.index.apply(shown_errors, baseline_errors);
    }
}

/// The result of classifying unmatched baseline entries after a CLI check.
pub struct BaselinePruningResult {
    pub unused_entry_count: usize,
    pub retained_entries: Vec<BaselineError>,
}

fn is_definitely_unused(
    matched: bool,
    checked: bool,
    try_exists: impl FnOnce() -> std::io::Result<bool>,
) -> bool {
    !matched && (checked || matches!(try_exists(), Ok(false)))
}

/// A baseline matcher that also retains rows and tracks matches for CLI maintenance actions.
pub struct TrackedBaselineProcessor {
    /// The baseline's rows, in the same order as `index.keys`.
    entries: Vec<BaselineError>,
    index: BaselineIndex,
}

impl TrackedBaselineProcessor {
    pub fn from_json(
        content: &str,
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
    ) -> Result<Self> {
        let baseline_file: BaselineErrors =
            serde_json::from_str(content).context(INVALID_BASELINE_GUIDANCE)?;
        Self::from_baseline_errors(baseline_file, relative_to, matching_mode)
            .context(INVALID_BASELINE_GUIDANCE)
    }

    fn from_baseline_errors(
        baseline_errors: BaselineErrors,
        relative_to: &Path,
        matching_mode: BaselineMatchingMode,
    ) -> Result<Self> {
        let index = BaselineIndex::new(&baseline_errors.errors, relative_to, matching_mode)?;
        Ok(Self {
            entries: baseline_errors.errors,
            index,
        })
    }

    /// Baseline suppressions are processed last, after inline and config suppressions.
    ///
    /// Unmatched rows are then classified conservatively using the scope of the current
    /// check. An unmatched row is unused only when its file was checked, or when the file
    /// is conclusively absent. Existing unchecked files and filesystem errors are
    /// retained. Duplicate rows sharing a key are classified individually.
    ///
    /// Under `column-ordered` a row is unused when the alignment found no counterpart
    /// for it. The set-based modes instead treat a single match as covering every row
    /// sharing that key, because there one row already suppresses any number of
    /// diagnostics.
    pub fn process_errors(
        self,
        shown_errors: &mut Vec<Error>,
        baseline_errors: &mut Vec<Error>,
        checked_paths: &HashSet<String>,
    ) -> BaselinePruningResult {
        let rows_matched = self.index.apply(shown_errors, baseline_errors);
        let mut unused_entry_count = 0;
        let retained_entries = self
            .entries
            .into_iter()
            .zip(&self.index.keys)
            .zip(rows_matched)
            .filter_map(|((entry, key), matched)| {
                let definitely_unused =
                    is_definitely_unused(matched, checked_paths.contains(&key.path), || {
                        Path::new(&key.path).try_exists()
                    });
                if definitely_unused {
                    unused_entry_count += 1;
                    None
                } else {
                    Some(entry)
                }
            })
            .collect();
        BaselinePruningResult {
            unused_entry_count,
            retained_entries,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Arc;

    use dupe::Dupe;
    use pyrefly_python::module::Module;
    use pyrefly_python::module_name::ModuleName;
    use pyrefly_python::module_path::ModulePath;
    use ruff_text_size::TextRange;
    use ruff_text_size::TextSize;

    use super::*;
    use crate::config::error_kind::ErrorKind;

    /// Whether the processor suppresses `error`, expressed through the public batch API.
    fn is_suppressed(processor: &BaselineProcessor, error: &Error) -> bool {
        let mut shown = vec![error.clone()];
        let mut baselined = Vec::new();
        processor.process_errors(&mut shown, &mut baselined);
        shown.is_empty()
    }

    #[test]
    fn test_definitely_unused_is_conservative_about_io_errors() {
        assert!(is_definitely_unused(false, true, || {
            Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "not consulted for checked paths",
            ))
        }));
        assert!(is_definitely_unused(false, false, || Ok(false)));
        assert!(!is_definitely_unused(false, false, || Ok(true)));
        assert!(!is_definitely_unused(false, false, || {
            Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "inconclusive",
            ))
        }));
        assert!(!is_definitely_unused(true, true, || Ok(false)));
    }

    #[test]
    fn test_baseline_key_generation() {
        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test/path.py")),
            Arc::new("test content".to_owned()),
        );

        let error = Error::new(
            module,
            TextRange::new(TextSize::new(0), TextSize::new(5)),
            "Test error message".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );

        let key = BaselineKey::from_error(&error, BaselineMatchingMode::Column);

        assert_eq!(key.path, "/workspace/test/path.py");
        assert_eq!(key.name, "bad-return");
        assert_eq!(key.matching_field, BaselineMatchingField::Column(1));
    }

    #[test]
    fn test_baseline_matching() {
        let baseline_json = r#"
        {
            "errors": [
                {
                    "line": 1,
                    "column": 3,
                    "stop_line": 1,
                    "stop_column": 5,
                    "path": "/workspace/test.py",
                    "code": -2,
                    "name": "bad-return",
                    "description": "Test error",
                    "concise_description": "Test error"
                }
            ]
        }
        "#;

        let baseline_file: BaselineErrors = serde_json::from_str(baseline_json).unwrap();
        let processor = BaselineProcessor::from_baseline_errors(
            baseline_file,
            Path::new("/workspace"),
            BaselineMatchingMode::Column,
        )
        .unwrap();

        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("test content 123456789".to_owned()),
        );
        let module2 = Module::new(
            ModuleName::from_str("test_module2"),
            ModulePath::filesystem(PathBuf::from("/workspace/test2.py")),
            Arc::new("test content 123456789".to_owned()),
        );

        // This error should match (same path, error code, and column)
        let error1 = Error::new(
            module.clone(),
            TextRange::new(TextSize::new(2), TextSize::new(5)),
            "Any error message".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        assert!(is_suppressed(&processor, &error1));

        // This error should not match (different column)
        let error2 = Error::new(
            module.clone(),
            TextRange::new(TextSize::new(4), TextSize::new(5)),
            "Test error".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        assert!(!is_suppressed(&processor, &error2));

        // This error should not match (different error code)
        let error3 = Error::new(
            module,
            TextRange::new(TextSize::new(2), TextSize::new(5)),
            "Any error message".to_owned(),
            Vec::new(),
            ErrorKind::AssertType,
        );
        assert!(!is_suppressed(&processor, &error3));

        // This error should not match (different module)
        let error4 = Error::new(
            module2.clone(),
            TextRange::new(TextSize::new(2), TextSize::new(5)),
            "Any error message".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        assert!(!is_suppressed(&processor, &error4));
    }

    #[test]
    fn test_baseline_matching_by_concise_description() {
        let baseline_json = r#"
        {
            "errors": [{
                "path": "/workspace/test.py",
                "name": "bad-return",
                "concise_description": "Expected description"
            }]
        }
        "#;
        let processor = BaselineProcessor::from_json(
            baseline_json,
            Path::new("/workspace"),
            BaselineMatchingMode::ConciseDescription,
        )
        .unwrap();
        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("test content 123456789".to_owned()),
        );

        let matching = Error::new(
            module.clone(),
            TextRange::new(TextSize::new(8), TextSize::new(10)),
            "Expected description".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        assert!(is_suppressed(&processor, &matching));

        let different_description = Error::new(
            module,
            TextRange::new(TextSize::new(0), TextSize::new(2)),
            "Different description".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        assert!(!is_suppressed(&processor, &different_description));
    }

    #[test]
    fn test_baseline_requires_the_configured_matching_field() {
        let column_only = r#"
        {"errors": [
            {"path": "valid.py", "name": "bad-return", "concise_description": "valid"},
            {"path": "test.py", "name": "bad-return", "column": 1}
        ]}
        "#;
        let err = BaselineProcessor::from_json(
            column_only,
            Path::new("/workspace"),
            BaselineMatchingMode::ConciseDescription,
        )
        .unwrap_err();
        let message = format!("{err:#}");
        assert!(message.contains("baseline file is invalid"));
        assert!(message.contains("baseline entry 2 (path `test.py`, error kind `bad-return`)"));
        assert!(message.contains("missing field `concise_description`"));
        assert!(message.contains("rerun with `--update-baseline`"));

        let description_only = r#"
        {
            "errors": [{
                "path": "test.py",
                "name": "bad-return",
                "concise_description": "test"
            }]
        }
        "#;
        let err = BaselineProcessor::from_json(
            description_only,
            Path::new("/workspace"),
            BaselineMatchingMode::Column,
        )
        .unwrap_err();
        assert!(format!("{err:#}").contains("missing field `column`"));
    }

    #[test]
    fn test_unused_entry_count() {
        let baseline_json = serde_json::json!({
            "errors": [
                {
                    "line": 1, "column": 3, "stop_line": 1, "stop_column": 5,
                    "path": "/workspace/test.py",
                    "code": -2, "name": "bad-return",
                    "description": "test", "concise_description": "test"
                },
                {
                    "line": 7, "column": 3, "stop_line": 7, "stop_column": 5,
                    "path": "/workspace/gone.py",
                    "code": -2, "name": "bad-return",
                    "description": "test", "concise_description": "test"
                }
            ]
        });
        let baseline_file: BaselineErrors = serde_json::from_value(baseline_json).unwrap();
        let processor = TrackedBaselineProcessor::from_baseline_errors(
            baseline_file,
            Path::new("/workspace"),
            BaselineMatchingMode::Column,
        )
        .unwrap();

        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("test content 123456789".to_owned()),
        );
        let mut shown_errors = vec![Error::new(
            module,
            TextRange::new(TextSize::new(2), TextSize::new(5)),
            "Any error message".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        )];
        let mut baseline_errors = Vec::new();
        let result = processor.process_errors(
            &mut shown_errors,
            &mut baseline_errors,
            &HashSet::from(["/workspace/test.py".to_owned()]),
        );

        assert!(shown_errors.is_empty());
        assert_eq!(baseline_errors.len(), 1);
        // The checked `test.py` entry matched, while the absent `gone.py` entry is stale.
        assert_eq!(result.unused_entry_count, 1);
    }

    #[test]
    fn test_duplicate_entries_are_counted_and_retained_individually() {
        // The same key appears twice for both `test.py` and `gone.py`, so the
        // baseline holds four raw rows across two unique keys.
        let baseline_json = serde_json::json!({
            "errors": [
                {
                    "line": 1, "column": 3, "stop_line": 1, "stop_column": 5,
                    "path": "/workspace/test.py",
                    "code": -2, "name": "bad-return",
                    "description": "first", "concise_description": "first"
                },
                {
                    "line": 1, "column": 3, "stop_line": 1, "stop_column": 5,
                    "path": "/workspace/test.py",
                    "code": -2, "name": "bad-return",
                    "description": "second", "concise_description": "second"
                },
                {
                    "line": 7, "column": 3, "stop_line": 7, "stop_column": 5,
                    "path": "/workspace/gone.py",
                    "code": -2, "name": "bad-return",
                    "description": "gone-a", "concise_description": "gone-a"
                },
                {
                    "line": 7, "column": 3, "stop_line": 7, "stop_column": 5,
                    "path": "/workspace/gone.py",
                    "code": -2, "name": "bad-return",
                    "description": "gone-b", "concise_description": "gone-b"
                }
            ]
        });
        let baseline_file: BaselineErrors = serde_json::from_value(baseline_json).unwrap();
        let processor = TrackedBaselineProcessor::from_baseline_errors(
            baseline_file,
            Path::new("/workspace"),
            BaselineMatchingMode::Column,
        )
        .unwrap();

        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("test content 123456789".to_owned()),
        );
        let mut shown_errors = vec![Error::new(
            module,
            TextRange::new(TextSize::new(2), TextSize::new(5)),
            "Any error message".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        )];
        let mut baseline_errors = Vec::new();
        let result = processor.process_errors(
            &mut shown_errors,
            &mut baseline_errors,
            &HashSet::from(["/workspace/test.py".to_owned()]),
        );

        // Both `gone.py` rows are unused even though they share a single key, so
        // the count reflects raw rows rather than unique keys.
        assert_eq!(result.unused_entry_count, 2);

        // The surviving entries are the two `test.py` rows, returned in file
        // order rather than as a single deduplicated key.
        assert_eq!(result.retained_entries.len(), 2);
        assert!(
            result
                .retained_entries
                .iter()
                .all(|e| e.path == "/workspace/test.py")
        );
    }

    /// Baseline rows given as `(path, column, error kind)`, in the order listed.
    fn baseline_rows(rows: &[(&str, usize, ErrorKind)]) -> BaselineErrors {
        serde_json::from_value(serde_json::json!({
            "errors": rows
                .iter()
                .map(|(path, column, kind)| serde_json::json!({
                    "column": column,
                    "path": path,
                    "name": kind.to_name()
                }))
                .collect::<Vec<_>>()
        }))
        .unwrap()
    }

    /// Baseline rows for one file, in the order `--update-baseline` writes them.
    fn baseline_of(columns: &[usize]) -> BaselineErrors {
        baseline_rows(&columns.map(|column| ("/workspace/test.py", *column, ErrorKind::BadReturn)))
    }

    /// One `bad-return` diagnostic per line of `path`, each at the given column.
    fn errors_in(path: &str, columns: &[usize]) -> Vec<Error> {
        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from(path)),
            Arc::new("aaaaaaaa\n".repeat(columns.len())),
        );
        columns
            .iter()
            .enumerate()
            .map(|(line, column)| {
                let start = TextSize::new(line as u32 * 9 + (*column as u32 - 1));
                Error::new(
                    module.dupe(),
                    TextRange::new(start, start + TextSize::new(1)),
                    "Any error message".to_owned(),
                    Vec::new(),
                    ErrorKind::BadReturn,
                )
            })
            .collect()
    }

    /// One diagnostic per line of `/workspace/test.py`, each at the given column.
    fn errors_at(columns: &[usize]) -> Vec<Error> {
        errors_in("/workspace/test.py", columns)
    }

    /// Run the processor and return the file and 1-indexed line of each diagnostic it
    /// reports, in source order.
    fn reported(processor: &BaselineProcessor, mut shown: Vec<Error>) -> Vec<(String, u32)> {
        let mut baselined = Vec::new();
        processor.process_errors(&mut shown, &mut baselined);
        let mut reported = shown.map(|error| {
            (
                error.path().as_path().to_string_lossy().into_owned(),
                error.display_range().start.line_within_cell().get(),
            )
        });
        reported.sort();
        reported
    }

    fn ordered_processor_for(baseline: BaselineErrors) -> BaselineProcessor {
        BaselineProcessor::from_baseline_errors(
            baseline,
            Path::new("/workspace"),
            BaselineMatchingMode::ColumnOrdered,
        )
        .unwrap()
    }

    /// Run the processor and return the 1-indexed lines of the diagnostics it reports.
    fn reported_lines(processor: &BaselineProcessor, mut shown: Vec<Error>) -> Vec<u32> {
        let mut baselined = Vec::new();
        processor.process_errors(&mut shown, &mut baselined);
        shown
            .iter()
            .map(|error| error.display_range().start.line_within_cell().get())
            .collect()
    }

    fn ordered_processor(columns: &[usize]) -> BaselineProcessor {
        BaselineProcessor::from_baseline_errors(
            baseline_of(columns),
            Path::new("/workspace"),
            BaselineMatchingMode::ColumnOrdered,
        )
        .unwrap()
    }

    /// The reason for aligning rather than tallying: a diagnostic inserted in the middle
    /// of a file is reported at the line it was added, not at some later line that shares
    /// its key. Here the baseline is `[col 3, col 5, col 3]` and a new `col 3` appears on
    /// line 2, between the first two rows.
    #[test]
    fn test_ordered_reports_the_inserted_diagnostic_not_a_later_one() {
        let processor = ordered_processor(&[3, 5, 3]);
        assert_eq!(
            reported_lines(&processor, errors_at(&[3, 3, 5, 3])),
            vec![2]
        );
    }

    #[test]
    fn test_ordered_suppresses_an_unchanged_file() {
        let processor = ordered_processor(&[3, 5, 3]);
        assert!(reported_lines(&processor, errors_at(&[3, 5, 3])).is_empty());
    }

    #[test]
    fn test_ordered_suppresses_when_diagnostics_have_been_fixed() {
        let processor = ordered_processor(&[3, 5, 3]);
        assert!(reported_lines(&processor, errors_at(&[3, 5])).is_empty());
    }

    /// A key the baseline does not record at all is reported wherever it appears.
    #[test]
    fn test_ordered_reports_an_unrecorded_key() {
        let processor = ordered_processor(&[3, 5]);
        assert_eq!(reported_lines(&processor, errors_at(&[3, 7, 5])), vec![2]);
    }

    /// Alignment cannot tell which member of a run of identical keys is new, so it falls
    /// back to blaming the last one. This is the known limit of a key that does not vary
    /// with the surrounding source.
    #[test]
    fn test_ordered_blames_the_last_of_an_identical_run() {
        let processor = ordered_processor(&[3, 3, 3]);
        assert_eq!(
            reported_lines(&processor, errors_at(&[3, 3, 3, 3])),
            vec![4]
        );
    }

    /// Each file is aligned against its own rows. Diagnostics arrive from two files in no
    /// particular order, and a new one in `a.py` leaves `b.py` untouched.
    #[test]
    fn test_ordered_aligns_each_file_independently() {
        let processor = ordered_processor_for(baseline_rows(&[
            ("/workspace/a.py", 3, ErrorKind::BadReturn),
            ("/workspace/a.py", 5, ErrorKind::BadReturn),
            ("/workspace/b.py", 3, ErrorKind::BadReturn),
        ]));
        let mut shown = errors_in("/workspace/b.py", &[3]);
        shown.extend(errors_in("/workspace/a.py", &[3, 3, 5]));
        assert_eq!(
            reported(&processor, shown),
            vec![("/workspace/a.py".to_owned(), 2)]
        );
    }

    /// A file with no rows at all reports every diagnostic, without disturbing a file
    /// that does have rows.
    #[test]
    fn test_ordered_reports_everything_in_a_file_without_rows() {
        let processor = ordered_processor_for(baseline_rows(&[(
            "/workspace/a.py",
            3,
            ErrorKind::BadReturn,
        )]));
        let mut shown = errors_in("/workspace/a.py", &[3]);
        shown.extend(errors_in("/workspace/new.py", &[3, 5]));
        assert_eq!(
            reported(&processor, shown),
            vec![
                ("/workspace/new.py".to_owned(), 1),
                ("/workspace/new.py".to_owned(), 2),
            ]
        );
    }

    /// Diagnostics at the same position are ordered by error kind, as the rows are, so
    /// they match however they happen to be emitted.
    #[test]
    fn test_ordered_matches_several_kinds_at_one_position() {
        let mut kinds = [ErrorKind::BadReturn, ErrorKind::BadAssignment];
        kinds.sort();
        let processor = ordered_processor_for(baseline_rows(
            &kinds.map(|kind| ("/workspace/test.py", 3, kind)),
        ));
        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("aaaaaaaa\n".to_owned()),
        );
        let range = TextRange::new(TextSize::new(2), TextSize::new(3));
        let shown = kinds
            .iter()
            .rev()
            .map(|kind| Error::new(module.dupe(), range, "err".to_owned(), Vec::new(), *kind))
            .collect();
        assert!(reported(&processor, shown).is_empty());
    }

    /// The cost of comparing by position: when two diagnostics swap places, one of them
    /// no longer lines up, so it is reported and its row goes stale. A set-based mode
    /// would match both.
    #[test]
    fn test_ordered_reports_one_of_two_swapped_diagnostics() {
        let processor = ordered_processor(&[3, 5]);
        assert_eq!(reported_lines(&processor, errors_at(&[5, 3])).len(), 1);

        let tracked = TrackedBaselineProcessor::from_baseline_errors(
            baseline_of(&[3, 5]),
            Path::new("/workspace"),
            BaselineMatchingMode::ColumnOrdered,
        )
        .unwrap();
        let result = tracked.process_errors(
            &mut errors_at(&[5, 3]),
            &mut Vec::new(),
            &HashSet::from(["/workspace/test.py".to_owned()]),
        );
        assert_eq!(result.unused_entry_count, 1);
    }

    /// Pruning keeps its conservative guard under `column-ordered`: rows for a file that
    /// was checked but has no diagnostics, or that no longer exists, are retired, while
    /// rows for an existing file outside the check are kept.
    #[test]
    fn test_ordered_pruning_retains_rows_for_unchecked_files() {
        let root = tempfile::tempdir().unwrap();
        let checked = root.path().join("checked.py");
        let kept = root.path().join("kept.py");
        std::fs::write(&kept, "").unwrap();
        let path = |file: &Path| file.to_string_lossy().into_owned();
        let (checked, kept, gone) = (
            path(&checked),
            path(&kept),
            path(&root.path().join("gone.py")),
        );
        let processor = TrackedBaselineProcessor::from_baseline_errors(
            baseline_rows(&[
                (&checked, 3, ErrorKind::BadReturn),
                (&kept, 3, ErrorKind::BadReturn),
                (&gone, 3, ErrorKind::BadReturn),
            ]),
            root.path(),
            BaselineMatchingMode::ColumnOrdered,
        )
        .unwrap();
        let result =
            processor.process_errors(&mut Vec::new(), &mut Vec::new(), &HashSet::from([checked]));
        assert_eq!(result.unused_entry_count, 2);
        assert_eq!(
            result.retained_entries.map(|entry| entry.path.clone()),
            vec![kept]
        );
    }

    /// A baseline written the way `--update-baseline` writes it matches the same
    /// diagnostics however they arrive, including several kinds at one position.
    #[test]
    fn test_ordered_round_trips_through_the_written_order() {
        let module = Module::new(
            ModuleName::from_str("test_module"),
            ModulePath::filesystem(PathBuf::from("/workspace/test.py")),
            Arc::new("aaaaaaaa\naaaaaaaa\n".to_owned()),
        );
        let at = |start: u32, kind: ErrorKind| {
            Error::new(
                module.dupe(),
                TextRange::new(TextSize::new(start), TextSize::new(start + 1)),
                "err".to_owned(),
                Vec::new(),
                kind,
            )
        };
        let mut errors = vec![
            at(11, ErrorKind::BadReturn),
            at(2, ErrorKind::BadReturn),
            at(2, ErrorKind::BadAssignment),
            at(11, ErrorKind::BadAssignment),
        ];
        errors.extend(errors_in("/workspace/other.py", &[3, 5]));

        let mut written = errors.clone();
        sort_by_source_position(&mut written);
        let processor = ordered_processor_for(BaselineErrors::from_errors(
            Path::new("/workspace"),
            &written,
        ));

        errors.reverse();
        assert!(reported(&processor, errors).is_empty());
    }

    /// The set-based modes keep their existing behaviour: one row absorbs any number
    /// of matching diagnostics.
    #[test]
    fn test_column_mode_still_absorbs_repeated_occurrences() {
        let processor = BaselineProcessor::from_baseline_errors(
            baseline_of(&[3]),
            Path::new("/workspace"),
            BaselineMatchingMode::Column,
        )
        .unwrap();
        assert!(reported_lines(&processor, errors_at(&[3, 3, 3])).is_empty());
    }

    #[test]
    fn test_ordered_retires_rows_the_alignment_did_not_match() {
        let processor = TrackedBaselineProcessor::from_baseline_errors(
            baseline_of(&[3, 5, 3]),
            Path::new("/workspace"),
            BaselineMatchingMode::ColumnOrdered,
        )
        .unwrap();

        let mut shown = errors_at(&[3, 5]);
        let mut baselined = Vec::new();
        let result = processor.process_errors(
            &mut shown,
            &mut baselined,
            &HashSet::from(["/workspace/test.py".to_owned()]),
        );
        assert!(shown.is_empty());
        assert_eq!(baselined.len(), 2);

        // Two of the three rows still align, so exactly one is retired.
        assert_eq!(result.unused_entry_count, 1);
        assert_eq!(result.retained_entries.len(), 2);
    }

    /// Check that an error matches a baseline entry regardless of how the path is stored,
    /// under both modes that key on the column.
    fn assert_baseline_path_matches(baseline_path: &str) {
        let cwd = std::env::current_dir().unwrap();
        let abs_path = cwd.join("src/foo.py");

        let baseline_json = serde_json::json!({
            "errors": [{
                "line": 1, "column": 5, "stop_line": 1, "stop_column": 10,
                "path": baseline_path,
                "code": -2, "name": "bad-return",
                "description": "test", "concise_description": "test"
            }]
        });

        let module = Module::new(
            ModuleName::from_str("foo"),
            ModulePath::filesystem(abs_path),
            Arc::new("test content 123456789".to_owned()),
        );
        let error = Error::new(
            module,
            TextRange::new(TextSize::new(4), TextSize::new(10)),
            "err".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        for matching_mode in [
            BaselineMatchingMode::Column,
            BaselineMatchingMode::ColumnOrdered,
        ] {
            let processor = BaselineProcessor::from_baseline_errors(
                serde_json::from_value(baseline_json.clone()).unwrap(),
                &cwd,
                matching_mode,
            )
            .unwrap();
            assert!(is_suppressed(&processor, &error), "{matching_mode:?}");
        }
    }

    #[test]
    fn test_baseline_matches_absolute_path() {
        let cwd = std::env::current_dir().unwrap();
        let abs_path = cwd.join("src/foo.py");
        assert_baseline_path_matches(&abs_path.to_string_lossy());
    }

    #[test]
    fn test_baseline_matches_relative_path() {
        assert_baseline_path_matches("src/foo.py");
    }

    /// Verify that backslash paths (Windows) match forward-slash baseline entries.
    #[test]
    fn test_baseline_matches_backslash_error_path() {
        let baseline_json = serde_json::json!({
            "errors": [{
                "line": 1, "column": 5, "stop_line": 1, "stop_column": 10,
                "path": "/workspace/src/foo.py",
                "code": -2, "name": "bad-return",
                "description": "test", "concise_description": "test"
            }]
        });

        // Simulate a Windows-style path with backslashes in the error.
        let module = Module::new(
            ModuleName::from_str("foo"),
            ModulePath::filesystem(PathBuf::from(r"\workspace\src\foo.py")),
            Arc::new("test content 123456789".to_owned()),
        );
        let error = Error::new(
            module,
            TextRange::new(TextSize::new(4), TextSize::new(10)),
            "err".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        for matching_mode in [
            BaselineMatchingMode::Column,
            BaselineMatchingMode::ColumnOrdered,
        ] {
            let processor = BaselineProcessor::from_baseline_errors(
                serde_json::from_value(baseline_json.clone()).unwrap(),
                Path::new("/workspace"),
                matching_mode,
            )
            .unwrap();
            assert!(is_suppressed(&processor, &error), "{matching_mode:?}");
        }
    }

    #[test]
    fn test_baseline_matches_with_non_cwd_relative_to() {
        let cwd = std::env::current_dir().unwrap();
        let abs_path = cwd.join("src/foo.py");
        let relative_to = cwd.join("src");

        let baseline_json = serde_json::json!({
            "errors": [{
                "line": 1, "column": 5, "stop_line": 1, "stop_column": 10,
                "path": "foo.py",
                "code": -2, "name": "bad-return",
                "description": "test", "concise_description": "test"
            }]
        });

        let module = Module::new(
            ModuleName::from_str("foo"),
            ModulePath::filesystem(abs_path),
            Arc::new("test content 123456789".to_owned()),
        );
        let error = Error::new(
            module,
            TextRange::new(TextSize::new(4), TextSize::new(10)),
            "err".to_owned(),
            Vec::new(),
            ErrorKind::BadReturn,
        );
        for matching_mode in [
            BaselineMatchingMode::Column,
            BaselineMatchingMode::ColumnOrdered,
        ] {
            let processor = BaselineProcessor::from_baseline_errors(
                serde_json::from_value(baseline_json.clone()).unwrap(),
                &relative_to,
                matching_mode,
            )
            .unwrap();
            assert!(is_suppressed(&processor, &error), "{matching_mode:?}");
        }
    }
}
