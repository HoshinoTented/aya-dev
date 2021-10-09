// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.pretty;

import org.aya.pretty.backend.string.LinkId;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Style;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

public class HtmlStyleTest {
  @Test public void testHtmlStyle() {
    assertFalse(doc().renderToHtml().isEmpty());
  }

  @Test public void testTexStyle() {
    assertFalse(doc().renderToTeX().isEmpty());
  }

  @NotNull private Doc doc() {
    var a = Doc.styled(Style.bold(), "bold");
    var b = Doc.styled(Style.italic(), "italic");
    var c = Doc.styled(Style.bold().and().italic().color(0xf08f68), "color1");
    var d = Doc.styled(Style.bold().and().italic().colorBG(0xf08f68), "color2");
    var sym = Doc.cat(Doc.symbol("=>"), Doc.symbol("hey"));
    var e = Doc.styled(Style.strike(), Doc.cat(a, b, c, d, sym));
    var f = Doc.cat(e, Doc.hyperLink("Click me", new LinkId("https://google.com")));
    return Doc.cat(f, Doc.hyperLink("Show dialog", new LinkId("javascript:alert('hello world');")));
  }
}
