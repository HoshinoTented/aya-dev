// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.

rootProject.name = "aya-prover"

dependencyResolutionManagement {
  @Suppress("UnstableApiUsage") repositories {
    mavenCentral()
  }
}

includeBuild("../guest0x0")
include(
  "cli",
  "parser",
  "tools",
  "tools-repl",
  "base",
  "pretty",
  "lsp",
)
