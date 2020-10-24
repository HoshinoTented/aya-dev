// Copyright (c) 2020-2020 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the Apache-2.0 license that can be found in the LICENSE file.
package org.mzi.api.core.def;

import org.jetbrains.annotations.ApiStatus;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.mzi.api.ref.Var;

/**
 * @author kiva
 */
@ApiStatus.NonExtendable
public interface CoreDef {
  @Contract(pure = true) @NotNull Var ref();
}
