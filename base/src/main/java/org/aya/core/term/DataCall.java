// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.term;

import kala.collection.immutable.ImmutableSeq;
import org.aya.concrete.stmt.TeleDecl;
import org.aya.core.def.CtorDef;
import org.aya.core.def.DataDef;
import org.aya.util.Arg;
import org.aya.ref.DefVar;
import org.jetbrains.annotations.NotNull;

public record DataCall(
  @Override @NotNull DefVar<DataDef, TeleDecl.DataDecl> ref,
  @Override int ulift,
  @Override @NotNull ImmutableSeq<Arg<@NotNull Term>> args
) implements Callable.DefCall, StableWHNF {

  public @NotNull ConCall.Head conHead(@NotNull DefVar<CtorDef, TeleDecl.DataCtor> ctorRef) {
    return new ConCall.Head(ref, ctorRef, ulift, args);
  }
}
