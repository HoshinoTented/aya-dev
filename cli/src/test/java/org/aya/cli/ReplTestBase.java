// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.cli;

import kala.collection.immutable.ImmutableSeq;
import kala.tuple.Tuple;
import kala.tuple.Tuple2;
import org.aya.cli.repl.AyaRepl;
import org.aya.cli.repl.ReplConfig;
import org.aya.repl.IO;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;

import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ReplTestBase {
  public static final @NotNull Path configDir = Paths.get("build", "test_config.json");
  public static final @NotNull ReplConfig config = new ReplConfig(configDir);

  @BeforeAll public static void setup() {
    config.enableUnicode = false;
    config.silent = true;
    config.prompt = "";
  }

  @NotNull protected Tuple2<String, String> repl(@NotNull String input) {
    var out = new StringWriter();
    var err = new StringWriter();
    var reader = new StringReader(input + "\n:exit");
    var repl = new AyaRepl.PlainRepl(ImmutableSeq.empty(), config, new IO(reader, out, err));
    repl.run();
    return Tuple.of(out.toString(), err.toString());
  }
}
