// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.cli;

import org.aya.cli.library.LibraryCompiler;
import org.aya.cli.library.incremental.CompilerAdvisor;
import org.aya.cli.repl.AyaRepl;
import org.aya.cli.repl.ReplConfig;
import org.aya.cli.single.CliReporter;
import org.aya.cli.single.CompilerFlags;
import org.aya.cli.single.SingleFileCompiler;
import org.aya.cli.utils.MainArgs;
import org.aya.core.def.PrimDef;
import org.aya.tyck.trace.MdUnicodeTrace;
import org.aya.tyck.trace.Trace;
import org.jetbrains.annotations.NotNull;
import picocli.CommandLine;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.concurrent.Callable;

public class Main extends MainArgs implements Callable<Integer> {
  public static void main(String... args) {
    System.exit(new CommandLine(new Main()).execute(args));
  }

  @Override public Integer call() throws Exception {
    if (action == null) {
      System.err.println("Try `aya --help` to see available commands");
      return 1;
    }
    if (action.repl != null)
      return AyaRepl.start(modulePaths().map(Paths::get), action.repl);
    assert action.compile != null;
    return doCompile(action.compile);
  }

  private int doCompile(@NotNull CompileAction compile) throws IOException {
    var message = asciiOnly
      ? CompilerFlags.Message.ASCII
      : CompilerFlags.Message.EMOJI;
    var inputFile = compile.inputFile;
    var outputFile = compile.outputFile;
    var filePath = Paths.get(inputFile);
    var outputPath = outputFile == null ? null : Paths.get(outputFile);
    var distillOptions = ReplConfig.loadFromDefault().distillerOptions;
    var reporter = CliReporter.stdio(!asciiOnly, distillOptions, verbosity);
    var distillation = prettyStage != null ? new CompilerFlags.DistillInfo(
      prettyStage,
      prettyFormat,
      Paths.get(prettyDir != null ? prettyDir : ".")
    ) : null;
    var flags = new CompilerFlags(message, interruptedTrace,
      compile.isRemake, distillation,
      modulePaths().view().map(Paths::get),
      outputPath);

    if (compile.isLibrary || compile.isRemake || compile.isNoCode) {
      // TODO: move to a new tool
      var advisor = compile.isNoCode ? CompilerAdvisor.inMemory() : CompilerAdvisor.onDisk();
      return LibraryCompiler.compile(new PrimDef.Factory(), reporter, flags, advisor, filePath);
    }
    var traceBuilder = enableTrace ? new Trace.Builder() : null;
    var compiler = new SingleFileCompiler(reporter, null, traceBuilder, distillOptions);
    var status = compiler.compile(filePath, flags, null);
    if (traceBuilder != null)
      System.err.println(new MdUnicodeTrace(2, distillOptions)
        .docify(traceBuilder).debugRender());
    return status;
  }
}
