// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.error;

import org.aya.concrete.stmt.TeleDecl;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Style;
import org.aya.util.distill.DistillerOptions;
import org.aya.util.error.SourcePos;
import org.jetbrains.annotations.NotNull;

public sealed interface PrimError extends TyckError {
  record NoResultType(@NotNull TeleDecl.PrimDecl prim) implements PrimError {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.sep(prim.toDoc(options),
        Doc.english("is expected to have a type"));
    }

    @Override public @NotNull SourcePos sourcePos() {
      return prim.sourcePos();
    }
  }

  record BadInterval(
    @NotNull SourcePos sourcePos,
    int integer
  ) implements TyckError {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.sep(Doc.english("The point"),
        Doc.styled(Style.code(), String.valueOf(integer)),
        Doc.english("does not live in interval"));
    }

    @Override public @NotNull Doc hint(@NotNull DistillerOptions options) {
      return Doc.sep(Doc.english("Did you mean: "),
        Doc.styled(Style.code(), "0"),
        Doc.plain("or"),
        Doc.styled(Style.code(), "1")
      );
    }
  }
}
