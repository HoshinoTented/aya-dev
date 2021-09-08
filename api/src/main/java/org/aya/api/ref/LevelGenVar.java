// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the GNU GPLv3 license that can be found in the LICENSE file.
package org.aya.api.ref;

import org.jetbrains.annotations.NotNull;

/**
 * @author ice1000
 * @apiNote used only in concrete syntax.
 */
public record LevelGenVar(@NotNull String name) implements Var {
}
