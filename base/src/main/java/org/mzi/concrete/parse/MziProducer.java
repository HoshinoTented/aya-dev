// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the GNU GPLv3 license that can be found in the LICENSE file.
package org.mzi.concrete.parse;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.glavo.kala.Tuple;
import org.glavo.kala.Tuple2;
import org.glavo.kala.collection.immutable.ImmutableSeq;
import org.glavo.kala.collection.immutable.ImmutableVector;
import org.glavo.kala.collection.mutable.Buffer;
import org.glavo.kala.traversable.Traversable;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;
import org.mzi.api.error.SourcePos;
import org.mzi.api.util.Assoc;
import org.mzi.concrete.Decl;
import org.mzi.concrete.Expr;
import org.mzi.concrete.Stmt;
import org.mzi.generic.Arg;
import org.mzi.generic.Modifier;
import org.mzi.generic.Pat;
import org.mzi.parser.MziBaseVisitor;
import org.mzi.parser.MziParser;
import org.mzi.ref.LocalVar;

import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author ice1000, kiva
 */
public final class MziProducer extends MziBaseVisitor<Object> {
  public static final @NotNull MziProducer INSTANCE = new MziProducer();

  private MziProducer() {
  }

  @TestOnly
  public static @NotNull Expr parseExpr(@NotNull @NonNls @Language("TEXT") String code) {
    return INSTANCE.visitExpr(MziParsing.parser(code).expr());
  }

  @TestOnly
  public static @NotNull ImmutableSeq<Stmt> parseStmt(@NotNull @NonNls @Language("TEXT") String code) {
    return INSTANCE.visitStmt(MziParsing.parser(code).stmt());
  }

  @TestOnly
  public static @NotNull Tuple2<Decl, ImmutableSeq<Stmt>> parseDecl(@NotNull @NonNls @Language("TEXT") String code) {
    return INSTANCE.visitDecl(MziParsing.parser(code).decl());
  }

  @Override public ImmutableSeq<Stmt> visitProgram(MziParser.ProgramContext ctx) {
    return ctx.stmt().stream().map(this::visitStmt).flatMap(Traversable::stream).collect(ImmutableSeq.factory());
  }

  @Override
  public @NotNull ImmutableSeq<Stmt> visitStmt(MziParser.StmtContext ctx) {
    var importCmd = ctx.importCmd();
    if (importCmd != null) return ImmutableSeq.of(visitImportCmd(importCmd));
    var openCmd = ctx.openCmd();
    if (openCmd != null) return visitOpenCmd(openCmd);
    var decl = ctx.decl();
    if (decl != null) {
      var result = visitDecl(decl);
      return result._2.prepended(result._1);
    }
    var mod = ctx.module();
    if (mod != null) return ImmutableSeq.of(visitModule(mod));
    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  @Override
  public @NotNull Tuple2<Decl, ImmutableSeq<Stmt>> visitDecl(MziParser.DeclContext ctx) {
    var accessibility = ctx.PRIVATE() == null ? Stmt.Accessibility.Public : Stmt.Accessibility.Private;
    var fnDecl = ctx.fnDecl();
    if (fnDecl != null) return Tuple2.of(visitFnDecl(fnDecl, accessibility), ImmutableSeq.of());
    var dataDecl = ctx.dataDecl();
    if (dataDecl != null) return visitDataDecl(dataDecl, accessibility);
    var structDecl = ctx.structDecl();
    if (structDecl != null) return Tuple2.of(visitStructDecl(structDecl, accessibility), ImmutableSeq.of());
    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  public Decl.@NotNull FnDecl visitFnDecl(MziParser.FnDeclContext ctx, Stmt.Accessibility accessibility) {
    var modifiers = ctx.fnModifiers().stream()
      .map(this::visitFnModifiers)
      .distinct()
      .collect(Collectors.toCollection(() -> EnumSet.noneOf(Modifier.class)));
    var assocCtx = ctx.assoc();
    var abuseCtx = ctx.abuse();

    return new Decl.FnDecl(
      sourcePosOf(ctx.ID()),
      accessibility,
      modifiers,
      assocCtx == null ? null : visitAssoc(assocCtx),
      ctx.ID().getText(),
      visitTelescope(ctx.tele().stream()),
      type(ctx.type(), sourcePosOf(ctx)),
      visitFnBody(ctx.fnBody()),
      abuseCtx == null ? ImmutableSeq.of() : visitAbuse(abuseCtx)
    );
  }

  public @NotNull ImmutableSeq<Expr.@NotNull Param> visitTelescope(Stream<MziParser.TeleContext> stream) {
    return stream
      .map(this::visitTele)
      .flatMap(Traversable::stream)
      .collect(ImmutableSeq.factory());
  }

  @Override
  public @NotNull ImmutableSeq<@NotNull Stmt> visitAbuse(MziParser.AbuseContext ctx) {
    return ctx.stmt().stream()
      .map(this::visitStmt)
      .flatMap(Traversable::stream)
      .collect(ImmutableSeq.factory());
  }

  @Override
  public @NotNull Expr visitFnBody(MziParser.FnBodyContext ctx) {
    return visitExpr(ctx.expr());
  }

  @Override
  public @NotNull Expr visitLiteral(MziParser.LiteralContext ctx) {
    if (ctx.CALM_FACE() != null) return new Expr.HoleExpr(sourcePosOf(ctx), "_", null);
    var id = ctx.ID();
    if (id != null) return new Expr.UnresolvedExpr(sourcePosOf(ctx), id.getText());
    var universe = ctx.UNIVERSE();
    if (universe != null) {
      String universeText = universe.getText();
      var univTrunc = universeText.substring(1, universeText.indexOf("T"));
      var hLevel = switch (univTrunc) {
        default -> Integer.parseInt(univTrunc.substring(0, univTrunc.length() - 1));
        case "h-", "h" -> -3;
        case "" -> throw new UnsupportedOperationException("TODO");
        case "oo-" -> Integer.MAX_VALUE;
      };
      var uLevel = visitOptNumber(universeText.substring(universeText.indexOf("e") + 1), 0);
      return new Expr.UnivExpr(sourcePosOf(ctx), uLevel, hLevel);
    }
    var set = ctx.SET_UNIV();
    if (set != null) {
      var text = set.getText().substring("\\Set".length());
      return new Expr.UnivExpr(sourcePosOf(ctx), visitOptNumber(text, 0), 0);
    }
    var prop = ctx.PROP();
    if (prop != null) return new Expr.UnivExpr(sourcePosOf(ctx), 0, -1);
    if (ctx.LGOAL() != null) {
      var fillingExpr = ctx.expr();
      var filling = fillingExpr == null ? null : visitExpr(fillingExpr);
      return new Expr.HoleExpr(sourcePosOf(ctx), null, filling);
    }
    var number = ctx.NUMBER();
    if (number != null) return new Expr.LitIntExpr(sourcePosOf(ctx), Integer.parseInt(number.getText()));
    var string = ctx.STRING();
    if (string != null) return new Expr.LitStringExpr(sourcePosOf(ctx), string.getText());
    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  public int visitOptNumber(@NotNull String number, int defaultVal) {
    return Optional.of(number)
      .filter(Predicate.not(String::isEmpty))
      .map(Integer::parseInt)
      .orElse(defaultVal);
  }

  @Override
  public @NotNull ImmutableSeq<Expr.@NotNull Param> visitTele(MziParser.TeleContext ctx) {
    var literal = ctx.literal();
    if (literal != null)
      return ImmutableSeq.of(new Expr.Param(sourcePosOf(ctx), new LocalVar("_"), visitLiteral(literal), true));
    var teleMaybeTypedExpr = ctx.teleMaybeTypedExpr();
    if (ctx.LPAREN() != null) return visitTeleMaybeTypedExpr(teleMaybeTypedExpr).apply(true);
    assert ctx.LBRACE() != null;
    return visitTeleMaybeTypedExpr(teleMaybeTypedExpr).apply(false);
  }

  @Override
  public @NotNull Function<Boolean, ImmutableSeq<Expr.Param>> visitTeleMaybeTypedExpr(MziParser.TeleMaybeTypedExprContext ctx) {
    var type = type(ctx.type(), sourcePosOf(ctx.ids()));
    return explicit -> visitIds(ctx.ids())
      .map(var -> new Expr.Param(sourcePosOf(ctx), new LocalVar(var), type, explicit))
      .collect(ImmutableSeq.factory());
  }

  public @NotNull Expr visitExpr(MziParser.ExprContext ctx) {
    if (ctx instanceof MziParser.AppContext app) return visitApp(app);
    if (ctx instanceof MziParser.ProjContext proj) return visitProj(proj);
    if (ctx instanceof MziParser.PiContext pi) return visitPi(pi);
    if (ctx instanceof MziParser.SigmaContext sig) return visitSigma(sig);
    if (ctx instanceof MziParser.LamContext lam) return visitLam(lam);
    if (ctx instanceof MziParser.ArrContext arr) return visitArr(arr);
    // TODO: match
    throw new UnsupportedOperationException("TODO: " + ctx.getClass());
  }

  @Override
  public @NotNull Expr visitArr(MziParser.ArrContext ctx) {
    var from = visitExpr(ctx.expr(0));
    var to = visitExpr(ctx.expr(1));
    return new Expr.PiExpr(
      sourcePosOf(ctx),
      false,
      new Expr.Param(sourcePosOf(ctx.expr(0)), new LocalVar("_"), from, true),
      to
    );
  }

  @Override
  public @NotNull Expr visitApp(MziParser.AppContext ctx) {
    var argument = ctx.argument();
    final var atom = ctx.atom();
    if (argument.isEmpty()) return visitAtom(atom);
    return new Expr.AppExpr(
      sourcePosOf(ctx),
      visitAtom(atom),
      argument.stream()
        .map(this::visitArgument)
        .collect(ImmutableSeq.factory())
    );
  }

  @Override
  public @NotNull Expr visitAtom(MziParser.AtomContext ctx) {
    var literal = ctx.literal();
    if (literal != null) return visitLiteral(literal);

    final var typed = ctx.typed();
    if (typed.size() == 1) return visitTyped(typed.get(0));
    return new Expr.TupExpr(
      sourcePosOf(ctx),
      typed.stream()
        .map(this::visitTyped)
        .collect(ImmutableVector.factory())
    );
  }

  @Override
  public @NotNull Expr visitTyped(MziParser.TypedContext ctx) {
    final var type = ctx.type();
    if (type == null) return visitExpr(ctx.expr());
    else return new Expr.TypedExpr(
      sourcePosOf(ctx),
      visitExpr(ctx.expr()),
      type(type, sourcePosOf(type))
    );
  }

  @Override
  public @NotNull Arg<Expr> visitArgument(MziParser.ArgumentContext ctx) {
    var atom = ctx.atom();
    if (atom != null) return Arg.explicit(visitAtom(atom));
    if (ctx.LBRACE() != null) return Arg.implicit(new Expr.TupExpr(sourcePosOf(ctx),
      ctx.typed().stream()
        .map(this::visitTyped)
        .collect(ImmutableVector.factory())));
    // TODO: . idFix
    throw new UnsupportedOperationException();
  }

  @Override
  public Expr.@NotNull LamExpr visitLam(MziParser.LamContext ctx) {
    return (Expr.LamExpr) buildLam(
      sourcePosOf(ctx),
      visitTelescope(ctx.tele().stream()),
      visitLamBody(ctx)
    );
  }

  public static @NotNull Expr buildLam(
    SourcePos sourcePos,
    ImmutableSeq<Expr.Param> params,
    Expr body
  ) {
    if (params.isEmpty()) return body;
    return new Expr.LamExpr(
      sourcePos,
      params.first(),
      buildLam(sourcePos, params.drop(1), body)
    );
  }

  private @NotNull Expr visitLamBody(@NotNull MziParser.LamContext ctx) {
    var bodyExpr = ctx.expr();

    if (bodyExpr == null) {
      var impliesToken = ctx.IMPLIES();
      var bodyHolePos = impliesToken == null
        ? sourcePosOf(ctx)
        : sourcePosOf(impliesToken);

      return new Expr.HoleExpr(bodyHolePos, null, null);
    }

    return visitExpr(bodyExpr);
  }

  @Override
  public Expr.@NotNull TelescopicSigmaExpr visitSigma(MziParser.SigmaContext ctx) {
    return new Expr.TelescopicSigmaExpr(
      sourcePosOf(ctx),
      false,
      visitTelescope(ctx.tele().stream()),
      visitExpr(ctx.expr())
    );
  }

  @Override
  public Expr.@NotNull PiExpr visitPi(MziParser.PiContext ctx) {
    return (Expr.PiExpr) buildPi(
      sourcePosOf(ctx),
      false,
      visitTelescope(ctx.tele().stream()),
      visitExpr(ctx.expr())
    );
  }

  public static @NotNull Expr buildPi(
    SourcePos sourcePos,
    boolean co,
    ImmutableSeq<Expr.Param> params,
    Expr body
  ) {
    if (params.isEmpty()) return body;
    return new Expr.PiExpr(
      sourcePos,
      co,
      params.first(),
      buildPi(sourcePos, co, params.drop(1), body)
    );
  }

  @Override
  public Expr.@NotNull ProjExpr visitProj(MziParser.ProjContext proj) {
    return new Expr.ProjExpr(
      sourcePosOf(proj),
      visitExpr(proj.expr()),
      Integer.parseInt(proj.NUMBER().getText())
    );
  }

  public @NotNull Tuple2<Decl, ImmutableSeq<Stmt>> visitDataDecl(MziParser.DataDeclContext ctx, Stmt.Accessibility accessibility) {
    var abuseCtx = ctx.abuse();
    var openAccessibility = ctx.PUBLIC() != null ? Stmt.Accessibility.Public : Stmt.Accessibility.Private;
    var data = new Decl.DataDecl(
      sourcePosOf(ctx.ID()),
      accessibility,
      ctx.ID().getText(),
      visitTelescope(ctx.tele().stream()),
      type(ctx.type(), sourcePosOf(ctx)),
      visitDataBody(ctx.dataBody()),
      abuseCtx == null ? ImmutableSeq.of() : visitAbuse(abuseCtx)
    );
    if (ctx.OPEN() != null) {
      return Tuple2.of(
        data,
        ImmutableSeq.of(new Stmt.OpenStmt(
          sourcePosOf(ctx),
          openAccessibility,
          ImmutableSeq.of(ctx.ID().getText()),
          Stmt.OpenStmt.UseHide.EMPTY
        ))
      );
    } else return Tuple2.of(data, ImmutableSeq.of());
  }

  public @NotNull Expr type(@Nullable MziParser.TypeContext typeCtx, SourcePos sourcePos) {
    return typeCtx == null
      ? new Expr.HoleExpr(sourcePos, null, null)
      : visitType(typeCtx);
  }

  private @NotNull Decl.DataBody visitDataBody(MziParser.DataBodyContext ctx) {
    if (ctx instanceof MziParser.DataCtorsContext dcc) return visitDataCtors(dcc);
    if (ctx instanceof MziParser.DataClausesContext dcc) return visitDataClauses(dcc);

    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  @Override
  public Decl.DataBody visitDataCtors(MziParser.DataCtorsContext ctx) {
    return new Decl.DataBody.Ctors(
      ctx.dataCtor().stream()
        .map(this::visitDataCtor)
        .collect(Buffer.factory())
    );
  }

  @Override
  public Decl.DataBody visitDataClauses(MziParser.DataClausesContext ctx) {
    var elim = visitElim(ctx.elim());
    // TODO[imkiva]: use var will compile, but IDEA shows error
    Buffer<Tuple2<Pat<Expr>, Decl.DataCtor>> clauses = ctx.dataCtorClause().stream()
      .map(this::visitDataCtorClause)
      .collect(Buffer.factory());
    return new Decl.DataBody.Clauses(elim, clauses);
  }

  @Override
  public Decl.@NotNull DataCtor visitDataCtor(MziParser.DataCtorContext ctx) {
    var elimCtx = ctx.elim();
    var elim = elimCtx == null
      ? Buffer.<String>of()
      : visitElim(elimCtx);

    return new Decl.DataCtor(
      sourcePosOf(ctx),
      ctx.ID().getText(),
      visitTelescope(ctx.tele().stream()),
      elim,
      ctx.clause().stream()
        .map(this::visitClause)
        .collect(Buffer.factory()),
      ctx.COERCE() != null
    );
  }

  @Override public @NotNull Tuple2<@NotNull Pat<Expr>, Decl.@NotNull DataCtor>
  visitDataCtorClause(MziParser.DataCtorClauseContext ctx) {
    return Tuple.of(
      visitPattern(ctx.pattern()),
      visitDataCtor(ctx.dataCtor())
    );
  }

  @Override
  public @NotNull Pat<Expr> visitPattern(MziParser.PatternContext ctx) {
    // TODO[imkiva]: use var will compile, but IDEA shows error
    ImmutableSeq<Pat.Atom<Pat<Expr>>> atoms = ctx.atomPattern().stream()
      .map(this::visitAtomPattern).collect(ImmutableSeq.factory());
    return new Pat.UnresolvedPat<>(
      atoms.first(),
      atoms.drop(1).collect(Buffer.factory()),
      ctx.ID() != null ? new LocalVar(ctx.ID().getText()) : null,
      type(ctx.type(), sourcePosOf(ctx))
    );
  }

  @Override
  public Pat.@NotNull Atom<Pat<Expr>> visitAtomPattern(MziParser.AtomPatternContext ctx) {
    if (ctx.LPAREN() != null) return new Pat.Atom.Tuple<>(visitPatterns(ctx.patterns()));
    if (ctx.LBRACE() != null) return new Pat.Atom.Braced<>(visitPatterns(ctx.patterns()));
    if (ctx.CALM_FACE() != null) return new Pat.Atom.CalmFace<>();
    var number = ctx.NUMBER();
    if (number != null) return new Pat.Atom.Number<>(Integer.parseInt(number.getText()));
    var id = ctx.ID();
    if (id != null) return new Pat.Atom.Bind<>(new LocalVar(id.getText()));

    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  @Override
  public @NotNull Buffer<@NotNull Pat<Expr>> visitPatterns(MziParser.PatternsContext ctx) {
    return ctx.pattern().stream()
      .map(this::visitPattern)
      .collect(Buffer.factory());
  }

  @Override
  public @NotNull Pat.Clause<Expr> visitClause(MziParser.ClauseContext ctx) {
    if (ctx.ABSURD() != null) return new Pat.Clause.Impossible<>();
    return new Pat.Clause.Possible<>(
      visitPatterns(ctx.patterns()),
      visitExpr(ctx.expr())
    );
  }

  @Override
  public Buffer<String> visitElim(MziParser.ElimContext ctx) {
    return ctx.ID().stream()
      .map(ParseTree::getText)
      .collect(Buffer.factory());
  }

  public @NotNull Decl visitStructDecl(MziParser.StructDeclContext ctx, Stmt.Accessibility accessibility) {
    // TODO: visit struct decl
    throw new UnsupportedOperationException();
  }

  @Override
  public @NotNull Expr visitType(@NotNull MziParser.TypeContext ctx) {
    return visitExpr(ctx.expr());
  }

  @Override
  public @NotNull Stmt visitImportCmd(MziParser.ImportCmdContext ctx) {
    final var id = ctx.ID();
    return new Stmt.ImportStmt(
      sourcePosOf(ctx),
      visitModuleName(ctx.moduleName()),
      id == null ? null : id.getText()
    );
  }

  @Override
  public @NotNull ImmutableSeq<Stmt> visitOpenCmd(MziParser.OpenCmdContext ctx) {
    var accessibility = ctx.PUBLIC() == null
      ? Stmt.Accessibility.Private
      : Stmt.Accessibility.Public;
    var useHide = ctx.useHide();
    var modName = visitModuleName(ctx.moduleName());
    var open = new Stmt.OpenStmt(
      sourcePosOf(ctx),
      accessibility,
      modName,
      useHide != null ? visitUseHide(useHide) : Stmt.OpenStmt.UseHide.EMPTY
    );
    if (ctx.IMPORT() != null) return ImmutableSeq.of(
      new Stmt.ImportStmt(sourcePosOf(ctx), modName, null),
      open
    );
    else return ImmutableSeq.of(open);
  }

  public Stmt.OpenStmt.UseHide visitUse(List<MziParser.UseContext> ctxs) {
    return new Stmt.OpenStmt.UseHide(
      ctxs.stream()
        .map(MziParser.UseContext::useHideList)
        .map(MziParser.UseHideListContext::ids)
        .flatMap(this::visitIds)
        .collect(ImmutableSeq.factory()),
      Stmt.OpenStmt.UseHide.Strategy.Using);
  }

  public Stmt.OpenStmt.UseHide visitHide(List<MziParser.HideContext> ctxs) {
    return new Stmt.OpenStmt.UseHide(
      ctxs.stream()
        .map(MziParser.HideContext::useHideList)
        .map(MziParser.UseHideListContext::ids)
        .flatMap(this::visitIds)
        .collect(ImmutableSeq.factory()),
      Stmt.OpenStmt.UseHide.Strategy.Hiding);
  }

  @Override
  public @NotNull Stmt.OpenStmt.UseHide visitUseHide(@NotNull MziParser.UseHideContext ctx) {
    var use = ctx.use();
    if (use != null) return visitUse(use);
    return visitHide(ctx.hide());
  }

  @Override
  public @NotNull Stmt.ModuleStmt visitModule(MziParser.ModuleContext ctx) {
    return new Stmt.ModuleStmt(
      sourcePosOf(ctx),
      ctx.ID().getText(),
      ctx.stmt().stream().map(this::visitStmt)
        .flatMap(Traversable::stream)
        .collect(ImmutableSeq.factory())
    );
  }

  @Override
  public @NotNull Stream<String> visitIds(MziParser.IdsContext ctx) {
    return ctx.ID().stream().map(ParseTree::getText);
  }

  @Override
  public @NotNull ImmutableSeq<@NotNull String> visitModuleName(MziParser.ModuleNameContext ctx) {
    return ctx.ID().stream()
      .map(ParseTree::getText)
      .collect(ImmutableSeq.factory());
  }

  @Override
  public @NotNull Assoc visitAssoc(MziParser.AssocContext ctx) {
    if (ctx.FIX() != null) return Assoc.Fix;
    if (ctx.FIXL() != null) return Assoc.FixL;
    if (ctx.FIXR() != null) return Assoc.FixR;
    if (ctx.INFIX() != null) return Assoc.Infix;
    if (ctx.INFIXL() != null) return Assoc.InfixL;
    if (ctx.INFIXR() != null) return Assoc.InfixR;
    if (ctx.TWIN() != null) return Assoc.Twin;
    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  @Override
  public @NotNull Modifier visitFnModifiers(MziParser.FnModifiersContext ctx) {
    if (ctx.ERASE() != null) return Modifier.Erase;
    if (ctx.INLINE() != null) return Modifier.Inline;
    throw new IllegalArgumentException(ctx.getClass() + ": " + ctx.getText());
  }

  private @NotNull SourcePos sourcePosOf(ParserRuleContext ctx) {
    var interval = ctx.getSourceInterval();
    var start = ctx.getStart();
    var end = ctx.getStop();
    return new SourcePos(
      interval.a,
      interval.b,
      start.getLine(),
      start.getCharPositionInLine(),
      end.getLine(),
      end.getCharPositionInLine()
    );
  }

  private @NotNull SourcePos sourcePosOf(TerminalNode node) {
    var token = node.getSymbol();
    var line = token.getLine();
    return new SourcePos(
      token.getStartIndex(),
      token.getStopIndex(),
      line,
      token.getCharPositionInLine(),
      line,
      token.getCharPositionInLine() + token.getText().length()
    );
  }
}
