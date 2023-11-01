// Copyright (c) 2020-2023 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.literate;

import kala.collection.immutable.ImmutableSeq;
import kala.control.Option;
import org.aya.cli.literate.FlclToken;
import org.aya.cli.literate.HighlightInfo;
import org.aya.cli.parse.FlclParser;
import org.aya.prettier.AyaPrettierOptions;
import org.aya.pretty.doc.Link;
import org.aya.util.error.SourceFile;
import org.aya.util.reporter.ThrowingReporter;
import org.junit.jupiter.api.Test;

import static org.aya.literate.HighlighterTester.*;

public class Ice1000HighlightTest {
  @Test
  public void test0() {
    var code = """
      keyword: data where;
      data: Int Nat;
      symbol: ->;
      ---------------------
      data Int where
        zero : Int
        succ : Int ≃ Int
      """;

    var fakeFile = new SourceFile("baka@ice1000", Option.none(), code);
    var parser = new FlclParser(new ThrowingReporter(AyaPrettierOptions.debug()), fakeFile);
    var highlight = parser.computeAst().tokens().map(FlclToken::toInfo).sorted();

    var tester = new HighlighterTester(code, highlight, ImmutableSeq.of(
      keyword(70, 73, "data"),
      ref(75, 77, "Int", HighlightInfo.DefKind.Data),
      keyword(79, 83, "where"),
      // no highlight for zero
      ref(94, 96, "Int", HighlightInfo.DefKind.Data),
      ref(107, 109, "Int", HighlightInfo.DefKind.Data),
      ref(113, 115, "Int", HighlightInfo.DefKind.Data)
    ));

    tester.defMap.put(Link.page("Int"), Option.some(HighlightInfo.DefKind.Data));
    tester.runTest();
  }
}
