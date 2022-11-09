// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core.repr;

import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.MutableLinkedHashMap;
import kala.collection.mutable.MutableMap;
import kala.control.Option;
import kala.tuple.Tuple;
import kala.tuple.Tuple2;
import org.aya.core.def.Def;
import org.aya.core.def.GenericDef;
import org.jetbrains.annotations.NotNull;

import static org.aya.core.repr.CodeShape.CtorShape;
import static org.aya.core.repr.CodeShape.DataShape;

/**
 * @author kiva
 */
public sealed interface AyaShape {
  @NotNull CodeShape codeShape();

  @NotNull AyaShape NAT_SHAPE = new AyaIntShape();
  @NotNull AyaShape LIST_SHAPE = new AyaListShape();
  @NotNull ImmutableSeq<AyaShape> LITERAL_SHAPES = ImmutableSeq.of(NAT_SHAPE, LIST_SHAPE);

  record AyaIntShape() implements AyaShape {
    public static final @NotNull CodeShape DATA_NAT = new DataShape(ImmutableSeq.empty(), ImmutableSeq.of(
      new CtorShape(CodeShape.MomentId.ZERO, ImmutableSeq.empty()),
      new CtorShape(CodeShape.MomentId.SUC, ImmutableSeq.of(CodeShape.ParamShape.explicit(CodeShape.TermShape.Call.justCall(0))))
    ));

    @Override public @NotNull CodeShape codeShape() {
      return DATA_NAT;
    }
  }

  record AyaListShape() implements AyaShape {
    public static final @NotNull CodeShape DATA_LIST = new DataShape(
      ImmutableSeq.of(CodeShape.ParamShape.anyLicit(new CodeShape.TermShape.Sort(null, 0))),
      ImmutableSeq.of(
        new CtorShape(CodeShape.MomentId.NIL, ImmutableSeq.empty()),
        new CtorShape(CodeShape.MomentId.CONS, ImmutableSeq.of(
          CodeShape.ParamShape.anyLicit(new CodeShape.TermShape.TeleRef(0, 0)),   // A
          CodeShape.ParamShape.anyLicit(new CodeShape.TermShape.Call(0, ImmutableSeq.of(    // List A
            new CodeShape.TermShape.TeleRef(0, 0))))))
      ));

    @Override public @NotNull CodeShape codeShape() {
      return DATA_LIST;
    }
  }

  class Factory {
    public @NotNull MutableMap<GenericDef, ShapeRecognition> discovered = MutableLinkedHashMap.of();

    public @NotNull ImmutableSeq<Tuple2<GenericDef, ShapeRecognition>> findImpl(@NotNull AyaShape shape) {
      return discovered.view().map(Tuple::of)
        .filter(t -> t._2.shape() == shape)
        .toImmutableSeq();
    }

    public @NotNull Option<ShapeRecognition> find(@NotNull Def def) {
      return discovered.getOption(def);
    }

    public void bonjour(@NotNull GenericDef def, @NotNull ShapeRecognition shape) {
      // TODO[literal]: what if a def has multiple shapes?
      discovered.put(def, shape);
    }

    /** Discovery of shaped literals */
    public void bonjour(@NotNull GenericDef def) {
      AyaShape.LITERAL_SHAPES.view()
        .flatMap(shape -> ShapeMatcher.match(shape, def))
        .forEach(shape -> bonjour(def, shape));
    }

    public void importAll(@NotNull Factory other) {
      discovered.putAll(other.discovered);
    }
  }
}
