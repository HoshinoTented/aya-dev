// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.cli.library;

import kala.collection.SeqLike;
import kala.collection.immutable.ImmutableSeq;
import org.aya.cli.library.source.LibrarySource;
import org.aya.concrete.stmt.Command;
import org.aya.concrete.stmt.Stmt;
import org.aya.util.error.SourcePos;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public record ImportResolver(@NotNull ImportLoader loader, @NotNull LibrarySource librarySource) {
  @FunctionalInterface
  public interface ImportLoader {
    @NotNull LibrarySource load(@NotNull ImmutableSeq<String> mod, @NotNull SourcePos sourcePos) throws IOException;
  }

  public void resolveStmt(@NotNull SeqLike<Stmt> stmts) throws IOException {
    stmts.forEachChecked(this::resolveStmt);
  }

  public void resolveStmt(@NotNull Stmt stmt) throws IOException {
    switch (stmt) {
      case Command.Module mod -> resolveStmt(mod.contents());
      case Command.Import cmd -> {
        var ids = cmd.path().ids();
        var success = loader.load(ids, cmd.sourcePos());
        librarySource.imports().append(success);
      }
      default -> {}
    }
  }
}
