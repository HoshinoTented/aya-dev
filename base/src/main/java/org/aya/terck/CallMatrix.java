// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.terck;

import kala.collection.immutable.ImmutableSeq;
import org.aya.core.term.CallTerm;
import org.aya.generic.util.InternalException;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Docile;
import org.aya.util.ArrayUtil;
import org.aya.util.Ordering;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.Debug;
import org.jetbrains.annotations.NotNull;

/**
 * A call matrix for a call `f --> g` has dimensions `arity(g) * arity(f)`.
 * Each row corresponds to one argument in the call to `g` (the codomain).
 * Each column corresponds to one formal argument of caller `f` (the domain).
 *
 * @author kiva
 * @see Relation
 */
@Debug.Renderer(text = "toDoc().debugRender()")
public record CallMatrix<Def, Param>(
  @NotNull CallTerm callTerm,
  @NotNull Def domain, @NotNull Def codomain,
  @NotNull ImmutableSeq<Param> domainTele,
  @NotNull ImmutableSeq<Param> codomainTele,
  @NotNull Relation[][] matrix
) implements Docile {
  public CallMatrix(
    @NotNull CallTerm callTerm,
    @NotNull Def domain, @NotNull Def codomain,
    @NotNull ImmutableSeq<Param> domainTele,
    @NotNull ImmutableSeq<Param> codomainTele
  ) {
    // TODO: sparse matrix?
    this(callTerm, domain, codomain, domainTele, codomainTele,
      new Relation[codomainTele.size()][domainTele.size()]);
    ArrayUtil.fill(matrix, Relation.unk());
  }

  public int rows() {
    return codomainTele.size();
  }

  public int cols() {
    return domainTele.size();
  }

  public void set(@NotNull Param domain, @NotNull Param codomain, @NotNull Relation relation) {
    int row = codomainTele.indexOf(codomain);
    int col = domainTele.indexOf(domain);
    assert row != -1;
    assert col != -1;
    matrix[row][col] = relation;
  }

  /**
   * A call matrix `A` is smaller than another call matrix `B` iff:
   * each relation in `A` is less than or equal to the corresponding relation in `B`.
   */
  public @NotNull Ordering compare(@NotNull CallMatrix<Def, Param> other) {
    if (this.domain != other.domain || this.codomain != other.codomain)
      throw new IllegalArgumentException("Cannot compare unrelated call matrices");
    if (this == other) return Ordering.Eq;
    for (int i = 0; i < rows(); i++)
      for (int j = 0; j < cols(); j++) {
        if (!this.matrix[i][j].lessThanOrEqual(other.matrix[i][j]))
          return Ordering.Gt;
      }
    return Ordering.Lt;
  }

  /**
   * Combine two call matrices if there exists an indirect call, for example:
   * If `f` calls `g` with call matrix `A` and `g` calls `h` with call matrix `B`,
   * the `f` indirectly calls `h` with call matrix `combine(A, B)` or `AB` in matrix notation.
   */
  @Contract(pure = true)
  public static <Def, Param> @NotNull CallMatrix<Def, Param> combine(
    @NotNull CallMatrix<Def, Param> A, @NotNull CallMatrix<Def, Param> B
  ) {
    if (B.domain != A.codomain) // implies B.cols() != A.rows()
      throw new InternalException("The combine cannot be applied to these two call matrices");

    var BA = new CallMatrix<>(B.callTerm, A.domain, B.codomain,
      A.domainTele, B.codomainTele);

    for (int i = 0; i < BA.rows(); i++)
      for (int j = 0; j < BA.cols(); j++)
        for (int k = 0; k < B.cols(); k++)
          BA.matrix[i][j] = BA.matrix[i][j].add(B.matrix[i][k].mul(A.matrix[k][j]));
    return BA;
  }

  public @NotNull Doc toDoc() {
    var lines = ImmutableSeq.from(matrix)
      .map(row -> Doc.stickySep(ImmutableSeq.from(row).map(Relation::toDoc)));
    return Doc.vcat(lines);
  }
}
