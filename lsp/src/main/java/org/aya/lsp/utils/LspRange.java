// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.lsp.utils;

import kala.control.Option;
import org.aya.util.error.SourcePos;
import org.javacs.lsp.Location;
import org.javacs.lsp.LocationLink;
import org.javacs.lsp.Position;
import org.javacs.lsp.Range;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.net.URI;
import java.nio.file.Path;

public class LspRange {
  public static final Range NONE = new Range();

  public static @NotNull Range toRange(@NotNull SourcePos sourcePos) {
    if (sourcePos == SourcePos.NONE) return NONE;
    return new Range(new Position(sourcePos.startLine() - 1, sourcePos.startColumn()),
      new Position(sourcePos.endLine() - 1, sourcePos.endColumn() + 1));
  }

  public static @NotNull Option<URI> fileUri(@NotNull SourcePos sourcePos) {
    return sourcePos.file().underlying().map(Path::toUri);
  }

  public static @Nullable LocationLink toLoc(@NotNull SourcePos from, @NotNull SourcePos to) {
    var uri = fileUri(to);
    if (uri.isEmpty()) return null;
    var fromRange = toRange(from);
    var toRange = toRange(to);
    return new LocationLink(fromRange, uri.get(), toRange, toRange);
  }

  public static @Nullable Location toLoc(@NotNull SourcePos to) {
    var uri = fileUri(to);
    if (uri.isEmpty()) return null;
    var toRange = toRange(to);
    return new Location(uri.get(), toRange);
  }
}
