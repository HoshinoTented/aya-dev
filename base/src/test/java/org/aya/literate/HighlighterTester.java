// Copyright (c) 2020-2023 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.literate;

import kala.collection.Seq;
import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.MutableMap;
import kala.control.Option;
import kala.tuple.Tuple;
import kala.tuple.Tuple2;
import kala.tuple.primitive.IntTuple2;
import org.aya.cli.literate.HighlightInfo;
import org.aya.cli.literate.SyntaxHighlight;
import org.aya.cli.parse.AyaParserImpl;
import org.aya.concrete.stmt.Stmt;
import org.aya.core.def.PrimDef.Factory;
import org.aya.literate.HighlighterTester.ExpectedType.LitInt;
import org.aya.pretty.doc.Link;
import org.aya.resolve.ResolveInfo;
import org.aya.resolve.context.EmptyContext;
import org.aya.test.AyaThrowingReporter;
import org.aya.test.EmptyModuleLoader;
import org.aya.util.error.SourceFile;
import org.aya.util.error.SourcePos;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

public class HighlighterTester {
  public record Expected(
    @NotNull SourcePos sourcePos,
    @NotNull HighlighterTester.ExpectedType expected
  ) implements Comparable<Expected> {
    @Override public int compareTo(@NotNull HighlighterTester.Expected o) {
      return sourcePos.compareTo(o.sourcePos);
    }
  }

  public sealed interface ExpectedType {
    @NotNull String display();

    record Def(
      @Override @NotNull String display, @Nullable String name,
      @Nullable HighlightInfo.DefKind kind
    ) implements ExpectedType {
    }

    record Ref(
      @Override @NotNull String display, @Nullable String name,
      @Nullable HighlightInfo.DefKind kind
    ) implements ExpectedType {}

    record Keyword(@Override @NotNull String display) implements ExpectedType {}

    record LitString(@NotNull String expected) implements ExpectedType {
      @Override
      public @NotNull String display() {
        return '\"' + expected + '\"';
      }
    }

    record LitInt(int expected) implements ExpectedType {
      @Override
      public @NotNull String display() {
        return Integer.toString(expected);
      }
    }
  }

  public final @NotNull String sourceCode;
  public final @NotNull ImmutableSeq<HighlightInfo> actual;
  public final @NotNull ImmutableSeq<@Nullable Expected> expected;

  // TODO[hoshino]: Inductive Defined (allow scope)
  // (User-Defined Name, (Unique ID, Nullable Def Kind))
  public final MutableMap<String, Tuple2<Link, Option<HighlightInfo.DefKind>>> userDefMap = MutableMap.create();
  public final MutableMap<Link, Option<HighlightInfo.DefKind>> defMap = MutableMap.create();

  public HighlighterTester(
    @NotNull String sourceCode,
    @NotNull ImmutableSeq<HighlightInfo> actual,
    @NotNull ImmutableSeq<@Nullable Expected> expected
  ) {
    this.sourceCode = sourceCode;
    this.actual = actual;
    this.expected = expected;
  }

  public void runTest() {
    var sortedActual = actual.view().distinct().sorted().toImmutableSeq().view();
    if (sortedActual.last() instanceof HighlightInfo.Lit(var $, var kind) && kind == HighlightInfo.LitKind.Eol)
      // Remove the last Eol
      sortedActual = sortedActual.dropLast(1);
    runTest(sortedActual.toImmutableSeq(), expected);
  }

  private void runTest(@NotNull ImmutableSeq<HighlightInfo> actuals, @NotNull ImmutableSeq<@Nullable Expected> expecteds) {
    actuals.forEachWith(expecteds, (actual, expected) -> {
      if (expected == null) {
        switch (actual) {
          case HighlightInfo.Def def -> checkDef(actual.sourcePos(), def);
          case HighlightInfo.Ref ref -> checkRef(actual.sourcePos(), ref);
          default -> {}
        }

        return;
      }

      assertEquals(
        IntTuple2.of(expected.sourcePos.tokenStartIndex(), expected.sourcePos.tokenEndIndex()),
        IntTuple2.of(actual.sourcePos().tokenStartIndex(), actual.sourcePos().tokenEndIndex()));

      var sourcePos = actual.sourcePos();
      var expectedText = expected.expected.display();
      var actualText = sourceCode.substring(sourcePos.tokenStartIndex(), sourcePos.tokenEndIndex() + 1);

      assertEquals(expectedText, actualText, "at " + sourcePos);

      switch (actual) {
        case HighlightInfo.Lit(var $, var ty)
          when ty == HighlightInfo.LitKind.Keyword && expected.expected() instanceof ExpectedType.Keyword -> {
        }
        case HighlightInfo.Lit(var $, var ty)
          when ty == HighlightInfo.LitKind.Int && expected.expected() instanceof LitInt -> {
        }
        case HighlightInfo.Lit(var $, var ty)
          when ty == HighlightInfo.LitKind.String && expected.expected() instanceof ExpectedType.LitString -> {
        }

        case HighlightInfo.Def def
          when expected.expected() instanceof ExpectedType.Def expectedDef ->
          assertDef(sourcePos, def, expectedDef);

        case HighlightInfo.Ref ref
          when expected.expected() instanceof ExpectedType.Ref expectedRef ->
          assertRef(sourcePos, ref, expectedRef);

        case HighlightInfo.Err err -> throw new UnsupportedOperationException("TODO");   // TODO

        default ->
          fail("expected: " + expected.expected.getClass().getSimpleName() + ", but actual: " + actual.getClass().getSimpleName());
      }
    });
    assertEquals(expecteds.size(), actuals.size(), "size mismatch");
  }

  /**
   * Check no duplicated def.
   */
  public void checkDef(@NotNull SourcePos sourcePos, @NotNull HighlightInfo.Def def) {
    var existDef = defMap.containsKey(def.target());
    assertFalse(existDef, "Duplicated def: " + def.target() + " at " + sourcePos);

    defMap.put(def.target(), Option.ofNullable(def.kind()));
  }

  public void assertDef(@NotNull SourcePos sourcePos, @NotNull HighlightInfo.Def actualDef, @NotNull HighlighterTester.ExpectedType.Def expectedDef) {
    checkDef(sourcePos, actualDef);

    assertEquals(expectedDef.kind(), actualDef.kind());

    var name = expectedDef.name();

    if (name != null) {
      var existName = userDefMap.getOption(name);
      assertFalse(existName.isDefined(), "Duplicated name: " + expectedDef.name());

      userDefMap.put(name, Tuple.of(actualDef.target(), Option.ofNullable(actualDef.kind())));
    }
  }

  /**
   * Check the reference
   */
  public void checkRef(@NotNull SourcePos sourcePos, @NotNull HighlightInfo.Ref ref) {
    var defData = defMap.getOrNull(ref.target());

    assertNotNull(defData, "Expected def: " + ref.target() + " at " + sourcePos);
    assertEquals(defData.getOrNull(), ref.kind());
  }

  public void assertRef(@NotNull SourcePos sourcePos, @NotNull HighlightInfo.Ref actualRef, @NotNull HighlighterTester.ExpectedType.Ref expectedRef) {
    checkRef(sourcePos, actualRef);

    var name = expectedRef.name();

    if (name != null) {
      var existName = userDefMap.getOption(name);
      assertTrue(existName.isDefined(), "Undefined name: " + expectedRef.name());

      var defData = existName.get();
      assertEquals(defData.component2().getOrNull(), actualRef.kind());
    }

    assertEquals(actualRef.kind(), expectedRef.kind());
  }

  public static void highlightAndTest(@Language("Aya") @NotNull String code, @Nullable Expected... expected) {
    var sourceFile = new SourceFile("test.aya", Option.none(), code);
    var reporter = AyaThrowingReporter.INSTANCE;

    var parser = new AyaParserImpl(reporter);
    var stmts = parser.program(sourceFile);
    var resolveInfo = new ResolveInfo(
      new Factory(),
      new EmptyContext(reporter, Path.of(".")).derive("main"),
      stmts
    );

    Stmt.resolve(stmts, resolveInfo, EmptyModuleLoader.INSTANCE);

    var result = SyntaxHighlight.highlight(null, Option.some(sourceFile), stmts)
      .filterNot(it -> it instanceof HighlightInfo.Lit(var $, var kind)
        && ignored.contains(kind));
    new HighlighterTester(code, result, ImmutableSeq.from(expected)).runTest();
  }

  static Seq<HighlightInfo.LitKind> ignored = Seq.of(HighlightInfo.LitKind.Eol, HighlightInfo.LitKind.Whitespace);

  /// region Helper

  private static @NotNull SourcePos mockPos(int start, int end) {
    return new SourcePos(SourceFile.NONE, start, end, 0, 0, 0, 0);
  }

  public static @NotNull HighlighterTester.Expected keyword(int begin, int end, @NotNull String display) {
    return new Expected(mockPos(begin, end), new ExpectedType.Keyword(display));
  }

  public static @NotNull HighlighterTester.Expected def(int begin, int end, @NotNull String display, @Nullable String name, @Nullable HighlightInfo.DefKind defKind) {
    return new Expected(mockPos(begin, end), new ExpectedType.Def(display, name, defKind));
  }

  public static @NotNull HighlighterTester.Expected def(int begin, int end, @NotNull String display, @Nullable HighlightInfo.DefKind defKind) {
    return def(begin, end, display, null, defKind);
  }

  public static @NotNull HighlighterTester.Expected localDef(int begin, int end, @NotNull String display, @Nullable String name) {
    return def(begin, end, display, name, HighlightInfo.DefKind.LocalVar);
  }

  public static @NotNull HighlighterTester.Expected localDef(int begin, int end, @NotNull String display) {
    return localDef(begin, end, display, null);
  }

  public static @NotNull HighlighterTester.Expected ref(int begin, int end, @NotNull String display, @Nullable String name, HighlightInfo.DefKind defKind) {
    return new Expected(mockPos(begin, end), new ExpectedType.Ref(display, name, defKind));
  }

  public static @NotNull HighlighterTester.Expected ref(int begin, int end, @NotNull String display, HighlightInfo.DefKind defKind) {
    return ref(begin, end, display, null, defKind);
  }

  public static @NotNull HighlighterTester.Expected localRef(int begin, int end, @NotNull String display, @Nullable String name) {
    return ref(begin, end, display, name, HighlightInfo.DefKind.LocalVar);
  }

  public static @NotNull HighlighterTester.Expected localRef(int begin, int end, @NotNull String display) {
    return localRef(begin, end, display, null);
  }

  public static @NotNull HighlighterTester.Expected litInt(int begin, int end, int display) {
    return new Expected(mockPos(begin, end), new LitInt(display));
  }

  @Contract(" -> null")
  public static @Nullable HighlighterTester.Expected whatever() {
    return null;
  }

  /// endregion
}
