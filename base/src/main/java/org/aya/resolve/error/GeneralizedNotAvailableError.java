// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.resolve.error;

import org.aya.api.distill.DistillerOptions;
import org.aya.api.ref.Var;
import org.aya.distill.BaseDistiller;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Style;
import org.aya.util.error.SourcePos;
import org.jetbrains.annotations.NotNull;

public record GeneralizedNotAvailableError(
  @Override @NotNull SourcePos sourcePos, @NotNull Var var
) implements ResolveProblem {
  @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
    return Doc.sep(
      Doc.english("The generalized variable"),
      Doc.styled(Style.code(), BaseDistiller.varDoc(var)),
      Doc.english("is not available here")
    );
  }

  @Override public @NotNull Severity level() {
    return Severity.ERROR;
  }
}
