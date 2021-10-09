// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.pretty;

import org.aya.pretty.backend.string.StringPrinterConfig;
import org.aya.pretty.doc.Doc;
import org.aya.pretty.doc.Style;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class Reproduction {
  @Test public void mock() {
    var doc = Doc.nest(2, Doc.styled(Style.code(), Doc.plain("hey")));
    assertEquals("  `hey'", doc.renderToString(StringPrinterConfig.unixTerminal()));
    assertEquals("\\hspace*{1.0em}\\fbox{hey}", doc.renderToTeX());
  }
}
