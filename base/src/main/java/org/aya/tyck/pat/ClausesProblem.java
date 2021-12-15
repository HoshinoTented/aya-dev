// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.pat;

import kala.collection.Seq;
import kala.collection.SeqView;
import kala.collection.immutable.ImmutableSeq;
import org.aya.api.distill.DistillerOptions;
import org.aya.api.error.Problem;
import org.aya.concrete.Pattern;
import org.aya.core.def.CtorDef;
import org.aya.core.pat.Pat;
import org.aya.core.term.CallTerm;
import org.aya.core.term.Term;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Style;
import org.aya.util.error.SourcePos;
import org.aya.util.error.WithPos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public sealed interface ClausesProblem extends Problem {
  @Override default @NotNull Severity level() {
    return Severity.ERROR;
  }

  private static @NotNull Doc termToHint(@Nullable Term term, @NotNull DistillerOptions options) {
    return term == null ? Doc.empty() : Doc.sep(Doc.english("substituted to"),
      Doc.styled(Style.code(), term.toDoc(options)));
  }

  record Conditions(
    @Override @NotNull SourcePos sourcePos,
    int i, int j,
    @NotNull Term lhs, @Nullable Term rhs,
    @NotNull SourcePos conditionPos,
    @NotNull SourcePos iPos, @Nullable SourcePos jPos
  ) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      var result = rhs != null ? Doc.sep(
        Doc.plain("unify"),
        Doc.styled(Style.code(), lhs.toDoc(options)),
        Doc.plain("and"),
        Doc.styled(Style.code(), rhs.toDoc(options))
      ) : Doc.english("even reduce one of the clause(s) to check condition");
      return Doc.sep(
        Doc.plain("The"),
        Doc.ordinal(i),
        Doc.english("clause matches on a constructor with condition(s). When checking the"),
        Doc.ordinal(j),
        Doc.english("condition, we failed to"),
        result
      );
    }

    @Override public @NotNull SeqView<WithPos<Doc>> inlineHints(@NotNull DistillerOptions options) {
      var view = Seq.of(
        new WithPos<>(conditionPos, Doc.plain("relevant condition")),
        new WithPos<>(iPos, termToHint(lhs, options))).view();
      return rhs == null || jPos == null ? view : view.concat(Seq.of(new WithPos<>(jPos, termToHint(rhs, options))));
    }
  }

  record Confluence(
    @Override @NotNull SourcePos sourcePos,
    int i, int j,
    @NotNull Term lhs, @NotNull Term rhs,
    @NotNull SourcePos iPos, @NotNull SourcePos jPos
  ) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.vcat(
        Doc.sep(
          Doc.plain("The"),
          Doc.ordinal(i),
          Doc.english("and the"),
          Doc.ordinal(j),
          Doc.english("clauses are not confluent because we failed to unify")),
        Doc.par(1, lhs.toDoc(options)),
        Doc.plain("and"),
        Doc.par(1, rhs.toDoc(options))
      );
    }

    @Override public @NotNull SeqView<WithPos<Doc>> inlineHints(@NotNull DistillerOptions options) {
      return Seq.of(new WithPos<>(iPos, termToHint(lhs, options)),
        new WithPos<>(jPos, termToHint(rhs, options))).view();
    }
  }

  /**
   * @author ice1000
   */
  record MissingCase(
    @Override @NotNull SourcePos sourcePos,
    @NotNull ImmutableSeq<Pattern> pats
  ) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.sep(Doc.english("Unhandled case:"), Doc.commaList(pats.map(t -> t.toDoc(options))));
    }
  }

  record SplitInterval(@Override @NotNull SourcePos sourcePos, @NotNull Pat pat) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.sep(
        Doc.english("Cannot perform pattern matching"),
        Doc.styled(Style.code(), pat.toDoc(options))
      );
    }
  }

  record UnsureCase(
    @Override @NotNull SourcePos sourcePos,
    @NotNull CtorDef ctor,
    @NotNull CallTerm.Data dataCall
  ) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.vcat(
        // Use `unsure` instead of `not sure`, which is used in Agda
        Doc.english("I'm unsure if there should be a case for constructor"),
        Doc.par(1, ctor.toDoc(options)),
        Doc.english("because I got stuck on the index unification of type"),
        Doc.par(1, dataCall.toDoc(options))
      );
    }
  }

  record Domination(int dom, int sub, @Override @NotNull SourcePos sourcePos) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      var subOrdinal = Doc.ordinal(sub);
      return Doc.sep(
        Doc.english("The"), Doc.ordinal(dom),
        Doc.english("clause dominates the"), subOrdinal,
        Doc.english("clause. The"), subOrdinal,
        Doc.english("clause will be unreachable")
      );
    }

    @Override public @NotNull Severity level() {
      return Severity.WARN;
    }
  }

  record FMDomination(int sub, @Override @NotNull SourcePos sourcePos) implements ClausesProblem {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      return Doc.sep(
        Doc.english("The"), Doc.ordinal(sub),
        Doc.english("clause is dominated by the other clauses, hence unreachable")
      );
    }

    @Override public @NotNull Severity level() {
      return Severity.WARN;
    }
  }
}
