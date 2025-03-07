/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import type {CollapsedFieldProps} from 'react-json-view';

import * as React from 'react';
import {useState, type MixedElement} from 'react';
import ReactJson from 'react-json-view';
import clsx from 'clsx';
import styles from './TryPyre2.module.css';

export type PyreflyErrorMessage = {
  startLineNumber: number,
  startColumn: number,
  endLineNumber: number,
  endColumn: number,
  message: string,
  severity: number,
};

component ErrorMessage(error: PyreflyErrorMessage) {
  const message =
    error.startLineNumber + ':' + error.startColumn + ': ' + error.message;
  return <span className={styles.msgType}>{message}</span>;
}

export default component TryPyre2Results(
  loading: boolean,
  errors: ?$ReadOnlyArray<PyreflyErrorMessage>,
  internalError: string,
) {
  const [activeToolbarTab, setActiveToolbarTab] = useState('errors');

  return (
    <div className={styles.results}>
      <div className={styles.toolbar}>
        <ul className={styles.tabs}>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'errors' && styles.selectedTab,
            )}
            onClick={() => setActiveToolbarTab('errors')}>
            Errors
          </li>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'json' && styles.selectedTab,
            )}
            onClick={() => setActiveToolbarTab('json')}>
            JSON
          </li>
        </ul>
      </div>
      {loading && (
        <div>
          <div className={styles.loader}>
            <div className={styles.bounce1}></div>
            <div className={styles.bounce2}></div>
            <div></div>
          </div>
        </div>
      )}
      {!loading && activeToolbarTab === 'errors' && (
        <pre className={clsx(styles.resultBody, styles.errors)}>
          <ul>
            {internalError ? (
              <li>TryPyrefly encountered an internal error: {internalError}</li>
            ) : errors === undefined || errors === null ? (
              <li>TryPyrefly failed to fetch errors</li>
            ) : errors?.length === 0 ? (
              <li>No errors!</li>
            ) : (
              errors.map((error, i) => (
                <li key={i}>
                  <ErrorMessage key={i} error={error} />
                </li>
              ))
            )}
          </ul>
        </pre>
      )}
      {!loading && activeToolbarTab === 'json' && (
        <pre className={styles.resultBody}>
          {JSON.stringify(errors, null, 2)}
        </pre>
      )}
    </div>
  );
}
