// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.error;

import org.aya.core.term.FormTerm;
import org.aya.pretty.doc.Doc;
import org.aya.util.distill.DistillerOptions;
import org.aya.util.error.SourcePos;
import org.aya.util.reporter.Problem;
import org.jetbrains.annotations.NotNull;

public record SortPiError(
  @Override @NotNull SourcePos sourcePos,
  @NotNull FormTerm.Sort domain,
  @NotNull FormTerm.Sort codomain
) implements Problem {
  @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
    return Doc.sepNonEmpty(Doc.english("Trying to check"),
      domain.toDoc(options),
      Doc.english("->"),
      codomain.toDoc(options));
  }

  @Override public @NotNull Severity level() {
    return Severity.ERROR;
  }
}
