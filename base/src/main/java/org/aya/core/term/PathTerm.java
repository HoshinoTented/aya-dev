// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.term;

import kala.collection.immutable.ImmutableSeq;
import org.aya.core.visitor.Subst;
import org.aya.util.Arg;
import org.aya.guest0x0.cubical.Partial;
import org.aya.ref.LocalVar;
import org.jetbrains.annotations.NotNull;

import java.util.function.Function;

/** generalized path type */
public record PathTerm(@NotNull Cube cube) implements StableWHNF {
  /**
   * 'Generalized path' syntax.
   *
   * @param params  Dimension parameters, never empty.
   * @param partial Partial element carried by this path.
   * @see org.aya.concrete.Expr.Path
   * @see PathTerm
   */
  public record Cube(
    @NotNull ImmutableSeq<LocalVar> params,
    @NotNull Term type,
    @NotNull Partial<Term> partial
  ) {
    public @NotNull Term eta(@NotNull Term term) {
      return new PLamTerm(params(), applyDimsTo(term)).rename();
    }

    public @NotNull PiTerm computePi() {
      var iTele = params().view().map(x -> new Param(x, IntervalTerm.INSTANCE, true));
      return (PiTerm) PiTerm.make(iTele, type());
    }

    public @NotNull Term applyDimsTo(@NotNull Term pLam) {
      var args = params.view().map(RefTerm::new);
      loop:
      while (true) {
        if (args.isEmpty()) return pLam;
        switch (pLam) {
          case default -> {
            break loop;
          }
          case PLamTerm lam -> {
            assert lam.params().sizeLessThanOrEquals(args);
            pLam = lam.body().subst(new Subst(lam.params(), args.take(lam.params().size())));
            args = args.drop(lam.params().size());
          }
          case LamTerm lam -> {
            // TODO: replace with error report¿
            assert lam.param().explicit();
            pLam = AppTerm.make(lam, new Arg<>(args.first(), true));
            args = args.drop(1);
          }
        }
      }
      var newArgs = args.map(x -> new Arg<Term>(x, true)).toImmutableSeq();
      return new PAppTerm(pLam, newArgs, this);
    }

    public @NotNull Cube map(@NotNull ImmutableSeq<LocalVar> params, @NotNull Function<Term, Term> mapper) {
      var ty = mapper.apply(type);
      var par = partial.map(mapper);
      if (ty == type && par == partial) return this;
      return new Cube(params, ty, par);
    }

    public @NotNull Cube map(@NotNull Function<Term, Term> mapper) {
      return map(params, mapper);
    }

    public @NotNull Term makeApp(@NotNull Term app, @NotNull Arg<Term> arg) {
      return AppTerm.make(etaLam(app), arg);
    }

    /** "not really eta". Used together with {@link #computePi()} */
    public @NotNull Term etaLam(@NotNull Term term) {
      return params.map(x -> new Param(x, IntervalTerm.INSTANCE, true))
        .foldRight(applyDimsTo(term), LamTerm::new).rename();
    }
  }
}
