package org.mzi.core.term;

import org.jetbrains.annotations.NotNull;
import org.mzi.tyck.sort.Sort;
import org.mzi.util.Decision;

/**
 * @author ice1000
 */
public record UnivTerm(@NotNull Sort sort) implements Term {
  @Override public <P, R> R accept(@NotNull Visitor<P, R> visitor, P p) {
    return visitor.visitUniv(this, p);
  }

  @Override public @NotNull Decision whnf() {
    return Decision.YES;
  }
}
