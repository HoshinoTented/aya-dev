// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.repl.gk;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.TokenType;
import org.aya.repl.Command;
import org.aya.repl.CommandArg;
import org.aya.repl.CommandManager;
import org.jetbrains.annotations.NotNull;
import org.jline.reader.*;
import org.jline.reader.impl.DefaultParser;

import java.util.Collections;
import java.util.List;

/**
 * @param shellLike see {@link org.aya.repl.CommandArg#shellLike}
 */
public record ReplParser(
  @NotNull CommandManager cmd, @NotNull FlexLexer lexer,
  @NotNull DefaultParser shellLike
) implements Parser {
  public ReplParser(@NotNull CommandManager cmd, @NotNull FlexLexer lexer) {
    this(cmd, lexer, new DefaultParser());
  }

  public record ReplParsedLine(
    @Override int wordCursor,
    @Override @NotNull List<@NotNull String> words,
    @Override @NotNull String word,
    @Override int wordIndex,
    @Override @NotNull String line,
    @Override int cursor
  ) implements CompletingParsedLine {
    @Override public CharSequence escape(CharSequence charSequence, boolean b) {
      return charSequence;
    }

    @Override public int rawWordCursor() {
      return wordCursor;
    }

    @Override public int rawWordLength() {
      return word.length();
    }
  }

  @Override public ParsedLine parse(String line, int cursor, ParseContext context) throws SyntaxError {
    if (line.isBlank()) return simplest(line, cursor, 0, Collections.emptyList());
    // ref: https://github.com/jline/jline3/issues/36
    if ((context == ParseContext.UNSPECIFIED || context == ParseContext.ACCEPT_LINE)
      && line.startsWith(Command.MULTILINE_BEGIN) && !line.endsWith(Command.MULTILINE_END)) {
      throw new EOFError(-1, cursor, "In multiline mode");
    }
    var trim = line.trim();
    if (trim.startsWith(Command.PREFIX)) {
      var shellAlike = cmd.parse(trim.substring(1)).command().view()
        .mapNotNull(CommandManager.CommandGen::argFactory)
        .anyMatch(CommandArg::shellLike);
      // ^ if anything matches
      if (shellAlike) return shellLike.parse(line, cursor, context);
    }
    // Drop whitespaces
    lexer.reset(line, 0, line.length(), 0);
    var tokens = lexer.allTheWayDown()
      .view()
      .filter(x -> x.type() != TokenType.WHITE_SPACE)
      .toImmutableSeq();
    var wordOpt = tokens.firstOption(token ->
      token.range().containsOffset(cursor));
    // In case we're in a whitespace or at the end
    if (wordOpt.isEmpty()) {
      var tokenOpt = tokens.firstOption(tok -> tok.range().getStartOffset() >= cursor);
      if (tokenOpt.isEmpty())
        return simplest(line, cursor, tokens.size(), tokens
          .map(tok -> textOf(line, tok)).asJava());
      var token = tokenOpt.get();
      var wordCursor = cursor - token.range().getStartOffset();
      return new ReplParsedLine(
        Math.max(wordCursor, 0), tokens.map(tok -> textOf(line, tok)).asJava(),
        textOf(line, token), tokens.size() - 1, line, cursor
      );
    }
    var word = wordOpt.get();
    var wordText = textOf(line, word);
    return new ReplParsedLine(
      cursor - word.range().getStartOffset(),
      tokens.stream().map(tok -> textOf(line, tok)).toList(),
      wordText, tokens.indexOf(word), line, cursor
    );
  }

  @NotNull private static String textOf(String line, FlexLexer.Token tok) {
    return tok.range().substring(line);
  }

  public @NotNull ReplParsedLine simplest(String line, int cursor, int wordIndex, List<@NotNull String> tokens) {
    return new ReplParsedLine(0, tokens, "", wordIndex, line, cursor);
  }
}
