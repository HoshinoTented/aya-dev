// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.resolve;

import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.MutableMap;
import kala.tuple.Tuple2;
import org.aya.concrete.desugar.AyaBinOpSet;
import org.aya.concrete.stmt.BindBlock;
import org.aya.concrete.stmt.Stmt;
import org.aya.concrete.stmt.UseHide;
import org.aya.core.def.PrimDef;
import org.aya.core.repr.AyaShape;
import org.aya.core.repr.CodeShape;
import org.aya.ref.DefVar;
import org.aya.resolve.context.ModuleContext;
import org.aya.tyck.order.TyckOrder;
import org.aya.util.binop.OpDecl;
import org.aya.util.error.SourcePos;
import org.aya.util.terck.MutableGraph;
import org.jetbrains.annotations.Debug;
import org.jetbrains.annotations.NotNull;

/**
 * @param primFactory  all primitives shared among all modules in a compilation task.
 * @param shapeFactory {@link CodeShape} that are discovered during tycking this module, modified by tycker.
 * @param opSet        binary operators.
 * @param opRename     rename-as-operators, only stores names that renamed in current module (and re-exported ops).
 * @param imports      modules imported using `import` command.
 * @param reExports    modules re-exported using `public open` command.
 * @param depGraph     dependency graph of definitions. for each (v, successors) in the graph,
 *                     `successors` should be tycked first.
 */
@Debug.Renderer(text = "thisModule.moduleName().joinToString(\"::\")")
public record ResolveInfo(
  @NotNull ModuleContext thisModule,
  @NotNull ImmutableSeq<Stmt> program,
  @NotNull PrimDef.Factory primFactory,
  @NotNull AyaShape.Factory shapeFactory,
  @NotNull AyaBinOpSet opSet,
  @NotNull MutableMap<DefVar<?, ?>, Tuple2<RenamedOpDecl, BindBlock>> opRename,
  @NotNull MutableMap<ImmutableSeq<String>, ResolveInfo> imports,
  @NotNull MutableMap<ImmutableSeq<String>, UseHide> reExports,
  @NotNull MutableGraph<TyckOrder> depGraph
) {
  public ResolveInfo(@NotNull PrimDef.Factory primFactory, @NotNull ModuleContext thisModule, @NotNull ImmutableSeq<Stmt> thisProgram, @NotNull AyaBinOpSet opSet) {
    this(thisModule, thisProgram, primFactory, new AyaShape.Factory(),
      opSet, MutableMap.create(), MutableMap.create(),
      MutableMap.create(), MutableGraph.create());
  }

  public void renameOp(@NotNull DefVar<?, ?> defVar, @NotNull RenamedOpDecl renamed, @NotNull BindBlock bind) {
    defVar.opDeclRename.put(thisModule().moduleName(), renamed);
    opRename.put(defVar, Tuple2.of(renamed, bind));
  }

  public void open(@NotNull ResolveInfo other, @NotNull SourcePos sourcePos, @NotNull Stmt.Accessibility acc) {
    // open defined operator and their bindings
    opSet().importBind(other.opSet(), sourcePos);
    // open discovered shapes as well
    shapeFactory().importAll(other.shapeFactory());
    // open renamed operators and their bindings
    other.opRename().forEach((defVar, tuple) -> {
      if (acc == Stmt.Accessibility.Public) {
        // if it is `public open`, make renamed operators transitively accessible by storing
        // them in my `opRename` bc "my importers" cannot access `other.opRename`.
        // see: https://github.com/aya-prover/aya-dev/issues/519
        renameOp(defVar, tuple._1, tuple._2);
      } else defVar.opDeclRename.put(thisModule().moduleName(), tuple._1);
    });
  }

  public record RenamedOpDecl(@NotNull OpInfo opInfo) implements OpDecl {
  }
}
