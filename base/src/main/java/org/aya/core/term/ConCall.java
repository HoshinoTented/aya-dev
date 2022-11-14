// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.term;

import kala.collection.immutable.ImmutableSeq;
import org.aya.concrete.stmt.TeleDecl;
import org.aya.core.def.CtorDef;
import org.aya.core.def.DataDef;
import org.aya.ref.DefVar;
import org.aya.util.Arg;
import org.jetbrains.annotations.NotNull;

import java.util.function.UnaryOperator;

public record ConCall(
  @NotNull ConCall.Head head,
  @NotNull ImmutableSeq<Arg<Term>> conArgs
) implements Callable.DefCall {
  public ConCall(
    @NotNull DefVar<DataDef, TeleDecl.DataDecl> dataRef,
    @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref,
    @NotNull ImmutableSeq<Arg<@NotNull Term>> dataArgs,
    int ulift,
    @NotNull ImmutableSeq<Arg<@NotNull Term>> conArgs
  ) {
    this(new Head(dataRef, ref, ulift, dataArgs), conArgs);
  }

  @Override public @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref() {
    return head.ref;
  }

  @Override public int ulift() {
    return head.ulift;
  }

  @Override public @NotNull ImmutableSeq<Arg<@NotNull Term>> args() {
    return head.dataArgs.view().concat(conArgs).toImmutableSeq();
  }

  /**
   * @param dataArgs the arguments to the data type, NOT the constructor patterns!!
   */
  public record Head(
    @NotNull DefVar<DataDef, TeleDecl.DataDecl> dataRef,
    @NotNull DefVar<CtorDef, TeleDecl.DataCtor> ref,
    int ulift,
    @NotNull ImmutableSeq<Arg<@NotNull Term>> dataArgs
  ) {
    public @NotNull DataCall underlyingDataCall() {
      return new DataCall(dataRef, ulift, dataArgs);
    }

    public @NotNull Head descent(@NotNull UnaryOperator<@NotNull Term> f) {
      var args = dataArgs.map(arg -> arg.descent(f));
      if (args.sameElements(dataArgs, true)) return this;
      return new Head(dataRef, ref, ulift, args);
    }
  }
}
