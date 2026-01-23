/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

use crate::commands::util::CommandExitStatus;

/// Suppress type errors by adding ignore comments, or remove unused ignores.
#[derive(Clone, Debug, Parser)]
pub struct SuppressArgs {}

impl SuppressArgs {
    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        println!("`pyrefly suppress` is still in development.");
        Ok(CommandExitStatus::Success)
    }
}
