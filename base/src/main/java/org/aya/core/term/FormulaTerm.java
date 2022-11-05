// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.term;

import kala.collection.SeqView;
import org.aya.guest0x0.cubical.Formula;
import org.aya.guest0x0.cubical.Restr;
import org.jetbrains.annotations.NotNull;

public record FormulaTerm(@NotNull Formula<Term> asFormula) implements Term {
  public static final @NotNull FormulaTerm LEFT = new FormulaTerm(new Formula.Lit<>(false));
  public static final @NotNull FormulaTerm RIGHT = new FormulaTerm(new Formula.Lit<>(true));

  public static @NotNull FormulaTerm inv(@NotNull Term term) {
    return new FormulaTerm(new Formula.Inv<>(term));
  }

  public static @NotNull FormulaTerm and(@NotNull Term lhs, @NotNull Term rhs) {
    return conn(true, lhs, rhs);
  }

  public static @NotNull FormulaTerm or(@NotNull Term lhs, @NotNull Term rhs) {
    return conn(false, lhs, rhs);
  }

  public static @NotNull FormulaTerm conn(boolean isAnd, @NotNull Term lhs, @NotNull Term rhs) {
    return new FormulaTerm(new Formula.Conn<>(isAnd, lhs, rhs));
  }

  public @NotNull Term simpl() {
    return Restr.formulae(asFormula(), FormulaTerm::new);
  }

  public @NotNull SeqView<Term> view() {
    return switch (asFormula) {
      case Formula.Conn<Term> cnn -> SeqView.of(cnn.l()).appended(cnn.r());
      case Formula.Inv<Term> inv -> SeqView.of(inv.i());
      case Formula.Lit<Term> lit -> SeqView.empty();
    };
  }
}
