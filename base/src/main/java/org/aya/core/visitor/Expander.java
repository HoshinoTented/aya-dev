// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.visitor;

import kala.collection.Set;
import kala.collection.mutable.MutableSet;
import org.aya.core.def.PrimDef;
import org.aya.core.term.*;
import org.aya.ref.AnyVar;
import org.aya.tyck.TyckState;
import org.jetbrains.annotations.NotNull;

public interface Expander extends DeltaExpander, BetaExpander {
  @Override @NotNull default Term post(@NotNull Term term) {
    return BetaExpander.super.post(DeltaExpander.super.post(term));
  }

  record Normalizer(@NotNull TyckState state) implements Expander {}

  record WHNFer(@NotNull TyckState state) implements Expander {
    @Override public @NotNull Term apply(@NotNull Term term) {
      return switch (term) {
        case IntroTerm intro -> intro;
        case FormTerm.Pi pi -> pi;
        case FormTerm.Sigma sigma -> sigma;
        case CallTerm.Data data -> data;
        case CallTerm.Struct struct -> struct;
        case CallTerm.Con con && (con.ref().core == null || con.ref().core.clauses.isEmpty()) -> con;
        default -> Expander.super.apply(term);
      };
    }
  }

  record Tracked(
    @NotNull Set<@NotNull AnyVar> unfolding,
    @NotNull MutableSet<@NotNull AnyVar> unfolded,
    @NotNull TyckState state,
    @NotNull PrimDef.Factory factory
  ) implements Expander {
    @Override public @NotNull Term apply(@NotNull Term term) {
      return switch (term) {
        case CallTerm.Fn fn -> {
          if (!unfolding.contains(fn.ref())) yield fn;
          unfolded.add(fn.ref());
          yield Expander.super.apply(fn);
        }
        case CallTerm.Con con -> {
          if (!unfolding.contains(con.ref())) yield con;
          unfolded.add(con.ref());
          yield Expander.super.apply(con);
        }
        case CallTerm.Prim prim -> factory.unfold(prim.id(), prim, state);
        default -> Expander.super.apply(term);
      };
    }
  }
}
