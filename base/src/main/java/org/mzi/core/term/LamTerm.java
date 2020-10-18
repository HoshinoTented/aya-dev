package org.mzi.core.term;

import org.jetbrains.annotations.NotNull;
import org.mzi.core.ref.Bind;

import java.util.List;

/**
 * @author ice1000
 */
public record LamTerm(@NotNull List<@NotNull Bind> binds, @NotNull Term body) {
}
