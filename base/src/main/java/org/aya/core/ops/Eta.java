// Copyright (c) 2020-2022 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.ops;

import kala.collection.immutable.ImmutableSeq;
import org.aya.core.term.*;
import org.aya.generic.Arg;
import org.aya.tyck.env.LocalCtx;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.VisibleForTesting;

public record Eta(@NotNull LocalCtx ctx) {
  /**
   * Note this is not a full eta-reduction. Several cases are ignored.
   * The overall goal is to determine whether a term can be reduced to RefTerm
   */
  public @NotNull Term uneta(@NotNull Term term) {
    return switch (term) {
      case IntroTerm.Lambda lam -> {
        var etaBody = uneta(lam.body());
        var last = getLastTerm(etaBody);
        var bodyWoLast = constructBodyWithoutLast(etaBody, last);
        if (last instanceof RefTerm lastRef
          && compareRefTerm(lam.param().toTerm(), lastRef)
          && bodyWoLast.findUsages(lastRef.var()) <= 0) yield uneta(bodyWoLast);
        yield IntroTerm.Lambda.make(ImmutableSeq.of(lam.param()), etaBody);
      }
      case IntroTerm.Tuple tuple -> {
        if (tuple.items().isEmpty()) yield tuple;
        var etaItems = tuple.items().map(this::uneta);
        var defaultRes = new IntroTerm.Tuple(etaItems);
        // Get first item's Proj.of Term to compare with other items'
        var firstItem = etaItems.first();
        if (!(firstItem instanceof ElimTerm.Proj proj
          && proj.of() instanceof RefTerm ref
          && ctx.get(ref.var()) instanceof FormTerm.Sigma sigmaTerm)) yield defaultRes;
        // Make sure targetSigma's size is equal to this tuple's size
        if (!sigmaTerm.params().sizeEquals(tuple.items().size())) yield defaultRes;
        // Make sure every Proj.of Term is the same and index match the position
        for (var i = 0; i < etaItems.size(); ++i) {
          var item = etaItems.get(i);
          if (!(item instanceof ElimTerm.Proj itemProj)
            || !compareRefTerm(itemProj.of(), ref)
            || (itemProj.ix() != i + 1)) yield defaultRes;
        }
        yield ref;
      }
      case ElimTerm.App app -> new ElimTerm.App(app.of(),
        new Arg<>(uneta(app.arg().term()), app.arg().explicit()));
      case ElimTerm.Proj proj -> new ElimTerm.Proj(uneta(proj.of()), proj.ix());
      // Ignore other cases because they are useless in becoming a RefTerm
      default -> term;
    };
  }

  private static @NotNull Term getLastTerm(@NotNull Term term) {
    return switch (term) {
      case IntroTerm.Lambda lam -> getLastTerm(lam.body());
      case ElimTerm.App app -> app.arg().term();
      default -> term;
    };
  }

  private static @NotNull Term constructBodyWithoutLast(@NotNull Term term, @NotNull Term lastTerm) {
    return switch (term) {
      case IntroTerm.Lambda lamTerm -> IntroTerm.Lambda.make(ImmutableSeq.of(lamTerm.param()),
        constructBodyWithoutLast(lamTerm.body(), lastTerm));
      case ElimTerm.App appTerm -> compareRefTerm(appTerm.arg().term(), lastTerm) ? appTerm.of() : appTerm;
      default -> term;
    };
  }

  @VisibleForTesting
  public static boolean compareRefTerm(@NotNull Term lhs, @NotNull Term rhs) {
    if (!(lhs instanceof RefTerm lhsRef && rhs instanceof RefTerm rhsRefTerm)) return false;
    return lhsRef.var() == rhsRefTerm.var();
  }
}
