// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.lsp.models;

import org.aya.core.term.Term;
import org.aya.lsp.utils.LspRange;
import org.aya.util.distill.DistillerOptions;
import org.aya.util.error.WithPos;
import org.javacs.lsp.Position;
import org.javacs.lsp.Range;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.net.URI;

/**
 * @param uri the uri to the file
 * @param computed null if failed
 */
public record ComputeTermResult(@NotNull URI uri, @Nullable String computed, @NotNull Range range) {
  public record Params(@NotNull URI uri, @NotNull Position position) {
  }

  public static @NotNull ComputeTermResult bad(@NotNull Params params) {
    return new ComputeTermResult(params.uri, null,
      new Range(params.position, params.position));
  }

  public static ComputeTermResult good(@NotNull Params params, @NotNull WithPos<Term> type) {
    return new ComputeTermResult(params.uri, type.data().toDoc(DistillerOptions.informative()).debugRender(),
      LspRange.toRange(type.sourcePos()));
  }
}
