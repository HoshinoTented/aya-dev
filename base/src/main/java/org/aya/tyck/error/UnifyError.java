// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.error;

import kala.collection.mutable.MutableList;
import org.aya.concrete.Expr;
import org.aya.core.term.Term;
import org.aya.generic.ExprProblem;
import org.aya.generic.util.NormalizeMode;
import org.aya.pretty.doc.Doc;
import org.aya.tyck.TyckState;
import org.aya.tyck.unify.Unifier;
import org.aya.util.distill.DistillerOptions;
import org.jetbrains.annotations.NotNull;

public interface UnifyError extends TyckError {
  @NotNull TyckState state();
  @NotNull Unifier.FailureData failureData();

  default @NotNull Doc describeUnify(
    @NotNull DistillerOptions options,
    @NotNull Doc prologue,
    @NotNull Term actual,
    @NotNull Doc epilogue,
    @NotNull Term expected
  ) {
    var actualDoc = actual.toDoc(options);
    var buf = MutableList.of(prologue, Doc.par(1, actualDoc));
    var actualNFDoc = actual.normalize(state(), NormalizeMode.NF).toDoc(options);
    if (!actualNFDoc.equals(actualDoc))
      buf.append(Doc.par(1, Doc.parened(Doc.sep(Doc.plain("Normalized:"), actualNFDoc))));
    var expectedDoc = expected.toDoc(options);
    buf.append(epilogue);
    buf.append(Doc.par(1, expectedDoc));
    var expectedNFDoc = expected.normalize(state(), NormalizeMode.NF).toDoc(options);
    if (!expectedNFDoc.equals(expectedDoc))
      buf.append(Doc.par(1, Doc.parened(Doc.sep(Doc.plain("Normalized:"), expectedNFDoc))));
    var failureLhs = failureData().lhs().freezeHoles(state()).toDoc(options);
    if (!failureLhs.equals(actualDoc)
      && !failureLhs.equals(expectedDoc)
      && !failureLhs.equals(actualNFDoc)
      && !failureLhs.equals(expectedNFDoc)
    ) buf.appendAll(new Doc[]{
      Doc.english("In particular, we failed to unify"),
      Doc.par(1, failureLhs),
      Doc.english("with"),
      Doc.par(1, failureData().rhs().freezeHoles(state()).toDoc(options))
    });
    return Doc.vcat(buf);
  }

  record Type(
    @Override @NotNull Expr expr,
    @NotNull Term expected,
    @NotNull Term actual,
    @Override @NotNull Unifier.FailureData failureData,
    @Override @NotNull TyckState state
  ) implements ExprProblem, UnifyError {
    @Override public @NotNull Doc describe(@NotNull DistillerOptions options) {
      var prologue = Doc.vcat(
        Doc.english("Cannot check the expression"),
        Doc.par(1, expr.toDoc(options)),
        Doc.english("of type"));
      return describeUnify(options, prologue, actual, Doc.english("against the type"), expected);
    }
  }
}
