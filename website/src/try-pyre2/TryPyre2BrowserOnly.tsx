/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import BrowserOnly from '@docusaurus/BrowserOnly';
import * as React from 'react';

const TryPyre2 = React.lazy(() => import('./TryPyre2'));

interface TryPyre2BrowserOnlyProps {
    sampleFilename: string;
    isCodeSnippet?: boolean;
    codeSample?: string;
    showErrorPanel?: boolean;
}

export default function TryPyre2BrowserOnly({
    sampleFilename,
    isCodeSnippet = false,
    codeSample = '',
    showErrorPanel = true,
}: TryPyre2BrowserOnlyProps): JSX.Element {
    if (sampleFilename == null) {
        throw 'Missing sampleFilename. IDE services won\'t work properly.';
    }
    return (
        <BrowserOnly>
            {() => (
                <React.Suspense fallback={<div>Loading...</div>}>
                    <TryPyre2
                        sampleFilename={sampleFilename}
                        isCodeSnippet={isCodeSnippet}
                        codeSample={codeSample}
                        showErrorPanel={showErrorPanel}
                    />
                </React.Suspense>
            )}
        </BrowserOnly>
    );
}
