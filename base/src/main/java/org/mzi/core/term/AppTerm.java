package org.mzi.core.term;

import asia.kala.collection.immutable.ImmutableSeq;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.mzi.core.subst.TermSubst;
import org.mzi.generic.Arg;
import org.mzi.ref.DefVar;
import org.mzi.util.Decision;

/**
 * @author ice1000
 * @see org.mzi.core.term.AppTerm#make(Term, Arg)
 */
public sealed interface AppTerm extends Term {
  @NotNull Term fn();
  @NotNull ImmutableSeq<@NotNull Arg<Term>> args();

  @Override default @NotNull Decision whnf() {
    if (fn() instanceof LamTerm) return Decision.NO;
    return fn().whnf();
  }

  @Contract(pure = true) static @NotNull Term make(@NotNull Term f, @NotNull Arg<Term> arg) {
    if (!(f instanceof LamTerm lam)) return new Apply(f, arg);
    var tele = lam.tele();
    var next = tele.next();
    return (next != null ? new LamTerm(next, lam.body()) : lam.body()).subst(new TermSubst(tele.ref(), arg.term()));
  }

  record FnCall(
    @NotNull DefVar fnRef,
    @NotNull ImmutableSeq<@NotNull Arg<Term>> args
  ) implements AppTerm {
    @Override public <P, R> R accept(@NotNull Visitor<P, R> visitor, P p) {
      return visitor.visitFnCall(this, p);
    }

    @Override public <P, Q, R> R accept(@NotNull BiVisitor<P, Q, R> visitor, P p, Q q) {
      return visitor.visitFnCall(this, p, q);
    }

    @Contract(value = " -> new", pure = true)
    @Override public @NotNull Term fn() {
      return new RefTerm(fnRef);
    }
  }

  record Apply(
    @NotNull Term fn,
    @NotNull Arg<Term> arg
  ) implements AppTerm {
    @Override public <P, R> R accept(@NotNull Visitor<P, R> visitor, P p) {
      return visitor.visitApp(this, p);
    }

    @Override public <P, Q, R> R accept(@NotNull BiVisitor<P, Q, R> visitor, P p, Q q) {
      return visitor.visitApp(this, p, q);
    }

    @Contract(" -> new")
    @Override public @NotNull ImmutableSeq<@NotNull Arg<Term>> args() {
      return ImmutableSeq.of(arg());
    }
  }
}
