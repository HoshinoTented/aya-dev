// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.parser;

import com.intellij.AyaModified;
import com.intellij.psi.builder.ASTMarkerVisitor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import kala.collection.SeqView;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

/** Generalized {@link ASTMarkerVisitor.Node} for reusing psi interfaces in Producer */
@AyaModified
public interface GenericNode<N extends GenericNode<N>> {
  @NotNull IElementType elementType();
  @NotNull String tokenText();
  int startOffset();
  int endOffset();
  boolean isTerminalNode();
  @NotNull SeqView<N> childrenView();
  default @NotNull @NonNls String toDebugString() {
    return toString();
  }

  default boolean is(@NotNull IElementType type) {
    return elementType() == type;
  }

  default boolean is(@NotNull TokenSet tokenSet) {
    return tokenSet.contains(elementType());
  }

  default @NotNull SeqView<N> childrenOfType(@NotNull IElementType type) {
    return childrenView().filter(c -> c.is(type));
  }

  default @NotNull SeqView<N> childrenOfType(@NotNull TokenSet tokenSet) {
    return childrenView().filter(c -> c.is(tokenSet));
  }

  default @Nullable N peekChild(@NotNull IElementType type) {
    return childrenOfType(type).firstOrNull();
  }

  default @Nullable N peekChild(@NotNull TokenSet tokenSet) {
    return childrenOfType(tokenSet).firstOrNull();
  }

  default @NotNull N child(@NotNull IElementType type) {
    return Objects.requireNonNull(peekChild(type));
  }

  default @NotNull N child(@NotNull TokenSet tokenSet) {
    return Objects.requireNonNull(peekChild(tokenSet));
  }
}
