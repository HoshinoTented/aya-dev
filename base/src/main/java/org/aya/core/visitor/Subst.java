// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.visitor;

import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.MutableHashMap;
import kala.collection.mutable.MutableMap;
import kala.collection.mutable.MutableTreeMap;
import org.aya.core.term.PrimTerm;
import org.aya.core.term.RefTerm;
import org.aya.core.term.Term;
import org.aya.distill.BaseDistiller;
import org.aya.generic.AyaDocile;
import org.aya.guest0x0.cubical.CofThy;
import org.aya.guest0x0.cubical.Formula;
import org.aya.guest0x0.cubical.Restr;
import org.aya.pretty.doc.Doc;
import org.aya.ref.AnyVar;
import org.aya.ref.LocalVar;
import org.aya.tyck.TyckState;
import org.aya.util.distill.DistillerOptions;
import org.jetbrains.annotations.Debug;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * "Substitution object"
 *
 * @author ice1000
 */
@Debug.Renderer(text = "map.toString()",
  childrenArray = "map.asJava().entrySet().toArray()",
  hasChildren = "map.isNotEmpty()")
public record Subst(
  @NotNull MutableMap<@NotNull AnyVar, @NotNull Term> map
) implements AyaDocile, CofThy.SubstObj<Term, LocalVar, Subst> {
  public static final @NotNull Subst EMPTY = new Subst(MutableTreeMap.of((o1, o2) -> {
    throw new UnsupportedOperationException("Shall not modify LevelSubst.EMPTY");
  }));

  public Subst() {
    this(MutableMap.create());
  }

  public Subst(@NotNull AnyVar var, @NotNull Term term) {
    this(MutableHashMap.of(var, term));
  }

  public Subst(@NotNull ImmutableSeq<LocalVar> from, @NotNull ImmutableSeq<Term> to) {
    this(MutableMap.from(from.zipView(to)));
  }

  public void subst(@NotNull Subst subst) {
    if (map.isEmpty()) return;
    map.replaceAll((var, term) -> term.subst(subst));
  }

  public ImmutableSeq<AnyVar> overlap(@NotNull Subst subst) {
    if (subst.map.isEmpty() || map.isEmpty()) return ImmutableSeq.empty();
    return map.keysView().filter(subst.map::containsKey).toImmutableSeq();
  }

  public @NotNull Subst addDirectly(@NotNull AnyVar var, @NotNull Term term) {
    map.put(var, term);
    return this;
  }

  public @NotNull Subst add(@NotNull AnyVar var, @NotNull Term term) {
    subst(new Subst(var, term));
    return addDirectly(var, term);
  }

  public @NotNull Subst add(@NotNull Subst subst) {
    if (subst.map.isEmpty()) return this;
    subst(subst);
    map.putAll(subst.map);
    return this;
  }

  public void clear() {
    map.clear();
  }

  public boolean isEmpty() {
    return map.isEmpty();
  }

  @Override public void put(LocalVar i, boolean isLeft) {
    map.put(i, isLeft ? PrimTerm.Mula.LEFT : PrimTerm.Mula.RIGHT);
  }

  @Override public boolean contradicts(LocalVar i, boolean newIsLeft) {
    // In an and-only cofibration, every variable appears uniquely in a cond.
    if (!map.containsKey(i)) return false;
    // check whether if the cond is self-contradictory
    if (!(map.get(i).asFormula() instanceof Formula.Lit<Term> end)) return false;
    return end.isLeft() != newIsLeft;
  }

  @Override public @Nullable LocalVar asRef(@NotNull Term term) {
    return term instanceof RefTerm ref ? ref.var() : null;
  }

  @Override public @NotNull Subst derive() {
    return new Subst(MutableMap.from(map));
  }

  public @NotNull Restr<Term> restr(@NotNull TyckState state, @NotNull Restr<Term> restr) {
    return Expander.restr(restr.fmap(t -> t.subst(this)));
  }

  @Override public @NotNull Doc toDoc(@NotNull DistillerOptions options) {
    return Doc.commaList(
      map.view().map((var, term) -> Doc.sep(
        BaseDistiller.varDoc(var),
        Doc.symbol("=>"),
        term.toDoc(options)
      )).toImmutableSeq()
    );
  }
}
