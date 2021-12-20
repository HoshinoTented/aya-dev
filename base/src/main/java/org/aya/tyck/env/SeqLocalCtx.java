// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.env;

import kala.collection.SeqView;
import kala.collection.mutable.DynamicSeq;
import org.aya.api.ref.LocalVar;
import org.aya.core.term.Term;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public record SeqLocalCtx(
  @NotNull DynamicSeq<P> localSeq,
  @Override @Nullable LocalCtx parent
) implements LocalCtx {
  public SeqLocalCtx() {
    this(DynamicSeq.create(), null);
  }

  public record P(@NotNull LocalVar var, @NotNull Term type) {
  }

  @Override public void remove(@NotNull SeqView<LocalVar> vars) {
    localSeq.removeAll(p -> vars.contains(p.var));
  }

  @Override public void extractToLocal(@NotNull DynamicSeq<Term.Param> dest) {
    localSeq.mapTo(dest, p -> new Term.Param(p.var, p.type, false));
  }

  @Override public @Nullable Term getLocal(@NotNull LocalVar var) {
    return localSeq.firstOption(p -> p.var.equals(var)).map(p -> p.type).getOrNull();
  }

  @Override public void put(@NotNull LocalVar var, @NotNull Term term) {
    localSeq.append(new P(var, term));
  }

  @Override public boolean isEmpty() {
    return localSeq.isEmpty();
  }
}