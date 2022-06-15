// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.tyck.order;

import kala.collection.immutable.ImmutableSeq;
import org.aya.concrete.stmt.Decl;
import org.aya.concrete.stmt.Stmt;
import org.aya.util.error.SourceNode;
import org.jetbrains.annotations.NotNull;

public sealed interface TyckUnit
  extends SourceNode
  permits Decl, Stmt {
  boolean needTyck(@NotNull ImmutableSeq<String> currentMod);
}
