package org.mzi.core.term;

import org.jetbrains.annotations.NotNull;
import org.mzi.core.ref.Ref;

/**
 * @author ice1000
 */
public record RefTerm(@NotNull Ref ref) implements Term {
}
