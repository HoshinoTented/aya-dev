// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.def;

import kala.collection.immutable.ImmutableSeq;
import org.aya.concrete.stmt.TeleDecl;
import org.aya.core.pat.Pat;
import org.aya.core.term.Term;
import org.aya.ref.DefVar;
import org.aya.tyck.ExprTycker;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * @author ice1000, kiva
 */
public final class CtorDef extends SubLevelDef {
  public final @NotNull DefVar<DataDef, TeleDecl.DataDecl> dataRef;
  public final @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref;
  public final @NotNull ImmutableSeq<Term.Matching> clauses;
  public final @NotNull ImmutableSeq<Pat> pats;

  public CtorDef(
    @NotNull DefVar<DataDef, TeleDecl.DataDecl> dataRef, @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref,
    @NotNull ImmutableSeq<Pat> pats,
    @NotNull ImmutableSeq<Term.Param> ownerTele, @NotNull ImmutableSeq<Term.Param> selfTele,
    @NotNull ImmutableSeq<Term.Matching> clauses, @NotNull Term result, boolean coerce
  ) {
    super(ownerTele, selfTele, result, coerce);
    ref.core = this;
    this.dataRef = dataRef;
    this.clauses = clauses;
    this.ref = ref;
    this.pats = pats;
  }

  public static @NotNull ImmutableSeq<Term.Param> conTele(@NotNull DefVar<CtorDef, TeleDecl.DataCtor> conVar) {
    if (conVar.core != null) return conVar.core.selfTele;
    else return Objects.requireNonNull(conVar.concrete.signature).param();
  }

  /**
   * @return first component: ctor's owner telescope, second component: ctor's self telescope
   */
  public static @NotNull DataDef.CtorTelescopes
  telescopes(@NotNull DefVar<CtorDef, TeleDecl.DataCtor> defVar) {
    var concrete = defVar.concrete;
    if (concrete != null) {
      // See the TeleDecl.DataCtor case of StmtTycker#tyckHeader
      var patternTele = concrete.patternTele;
      var signature = concrete.signature();

      assert patternTele != null;
      assert signature != null;

      return new DataDef.CtorTelescopes(patternTele, signature.param());
    }
    return new DataDef.CtorTelescopes(defVar.core.ownerTele, defVar.core.selfTele);
  }

  public @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref() {
    return ref;
  }

  @Override public @NotNull ImmutableSeq<Term.Param> telescope() {
    return fullTelescope().toImmutableSeq();
  }
}
