// Copyright (c) 2020-2023 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.cli.literate;

import kala.collection.immutable.ImmutableSeq;
import kala.text.StringSlice;
import org.aya.pretty.doc.Link;
import org.aya.util.error.SourcePos;
import org.jetbrains.annotations.NotNull;

public record FlclToken(
  @NotNull SourcePos range,
  @NotNull String text,
  @NotNull Type type
) {
  public static final @NotNull Link EMPTY_LINK = Link.page("");

  public record File(
    @NotNull ImmutableSeq<FlclToken> tokens,
    @NotNull StringSlice sourceCode,
    int startIndex
  ) {}

  public enum Type {
    Keyword, Fn, Data, Number, Local, Comment, Symbol
  }

  public @NotNull HighlightInfo toInfo() {
    return switch (type) {
      case Keyword -> new HighlightInfo.Lit(range, HighlightInfo.LitKind.Keyword);
      case Number -> new HighlightInfo.Lit(range, HighlightInfo.LitKind.Int);
      case Comment -> new HighlightInfo.Lit(range, HighlightInfo.LitKind.Comment);
      case Symbol -> new HighlightInfo.Lit(range, HighlightInfo.LitKind.SpecialSymbol);
      case Fn -> createRef(HighlightInfo.DefKind.Fn);
      case Data -> createRef(HighlightInfo.DefKind.Data);
      case Local -> createRef(HighlightInfo.DefKind.LocalVar);
    };
  }

  private @NotNull HighlightInfo.Ref createRef(HighlightInfo.@NotNull DefKind kind) {
    return new HighlightInfo.Ref(range, Link.page(text), kind, null);
  }
}
