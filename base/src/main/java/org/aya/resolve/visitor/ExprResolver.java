// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.resolve.visitor;

import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.MutableLinkedHashMap;
import kala.collection.mutable.MutableList;
import kala.collection.mutable.MutableMap;
import kala.collection.mutable.MutableStack;
import kala.value.MutableValue;
import org.aya.concrete.Expr;
import org.aya.concrete.Pattern;
import org.aya.concrete.stmt.GeneralizedVar;
import org.aya.concrete.stmt.TeleDecl;
import org.aya.concrete.visitor.EndoExpr;
import org.aya.concrete.visitor.EndoPattern;
import org.aya.core.def.CtorDef;
import org.aya.core.def.PrimDef;
import org.aya.generic.util.InternalException;
import org.aya.ref.AnyVar;
import org.aya.ref.DefVar;
import org.aya.ref.LocalVar;
import org.aya.resolve.context.Context;
import org.aya.resolve.error.GeneralizedNotAvailableError;
import org.aya.resolve.error.PrimResolveError;
import org.aya.tyck.error.FieldError;
import org.aya.tyck.order.TyckOrder;
import org.aya.tyck.order.TyckUnit;
import org.aya.util.error.SourcePos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

/**
 * Resolves bindings.
 *
 * @param allowedGeneralizes will be filled with generalized vars if allowGeneralized,
 *                           and represents the allowed generalized level vars otherwise
 * @author re-xyr, ice1000
 * @implSpec allowedGeneralizes must be linked map
 * @see StmtResolver
 */
public record ExprResolver(
  @NotNull Context ctx,
  @NotNull Options options,
  @NotNull MutableMap<GeneralizedVar, Expr.Param> allowedGeneralizes,
  @NotNull MutableList<TyckOrder> reference,
  @NotNull MutableStack<Where> where,
  @Nullable Consumer<TyckUnit> parentAdd
) implements EndoExpr {
  public ExprResolver(@NotNull Context ctx, @NotNull Options options) {
    this(ctx, options, MutableLinkedHashMap.of(), MutableList.create(), MutableStack.create(), null);
  }

  @NotNull Expr.PartEl partial(@NotNull Context ctx, Expr.PartEl el) {
    var partial = el.clauses().map(e ->
      Tuple.of(resolve(e._1, ctx), resolve(e._2, ctx)));
    if (partial.zipView(el.clauses())
      .allMatch(p -> p._1._1 == p._2._1 && p._1._2 == p._2._2)) return el;
    return new Expr.PartEl(el.sourcePos(), partial);
  }

  public void enterHead() {
    where.push(Where.Head);
    reference.clear();
  }

  public void enterBody() {
    where.push(Where.Body);
    reference.clear();
  }

  public @NotNull ExprResolver enter(Context ctx) {
    return ctx == ctx() ? this : new ExprResolver(ctx, options, allowedGeneralizes, reference, where, parentAdd);
  }

  public @NotNull ExprResolver member(@NotNull TyckUnit decl) {
    return new ExprResolver(ctx, RESTRICTIVE, allowedGeneralizes, MutableList.of(new TyckOrder.Head(decl)), MutableStack.create(),
      this::addReference);
  }

  public @NotNull ExprResolver body() {
    return new ExprResolver(ctx, RESTRICTIVE, allowedGeneralizes, reference, MutableStack.create(), this::addReference);
  }

  @Override public @NotNull Expr pre(@NotNull Expr expr) {
    return switch (expr) {
      case Expr.RawProj(var pos, var tup, var id, var resolved, var coeLeft, var restr) -> {
        var resolvedIx = ctx.getMaybe(id);
        if (resolvedIx == null)
          ctx.reportAndThrow(new FieldError.UnknownField(id.sourcePos(), id.join()));
        yield resolvedIx == resolved ? expr
          : new Expr.RawProj(pos, tup, id, resolvedIx, coeLeft, restr);
      }
      case Expr.Hole hole -> {
        hole.accessibleLocal().set(ctx.collect(MutableList.create()).toImmutableSeq());
        yield hole;
      }
      default -> EndoExpr.super.pre(expr);
    };
  }

  /**
   * Special handling of terms with binding structure.
   * We need to invoke a resolver with a different context under the binders.
   */
  @Override public @NotNull Expr apply(@NotNull Expr expr) {
    return switch (expr) {
      case Expr.Do doExpr -> doExpr.update(apply(doExpr.bindName()), resolve(doExpr.binds(), MutableValue.create(ctx)));
      case Expr.Match match -> {
        var clauses = match.clauses().map(this::apply);
        yield match.update(match.discriminant().map(this), clauses);
      }
      case Expr.New neu -> neu.update(apply(neu.struct()), neu.fields().map(field -> {
        var fieldCtx = field.bindings().foldLeft(ctx, (c, x) -> c.bind(x.data(), x.sourcePos()));
        return field.descent(enter(fieldCtx));
      }));
      case Expr.Lambda lam -> {
        var mCtx = MutableValue.create(ctx);
        var param = resolve(lam.param(), mCtx);
        yield lam.update(param, enter(mCtx.get()).apply(lam.body()));
      }
      case Expr.Pi pi -> {
        var mCtx = MutableValue.create(ctx);
        var param = resolve(pi.param(), mCtx);
        yield pi.update(param, enter(mCtx.get()).apply(pi.last()));
      }
      case Expr.Sigma sigma -> {
        var mCtx = MutableValue.create(ctx);
        var params = sigma.params().map(param -> resolve(param, mCtx));
        yield sigma.update(params);
      }
      case Expr.Path path -> {
        var newCtx = path.params().foldLeft(ctx, (c, x) -> c.bind(x, x.definition()));
        yield path.descent(enter(newCtx));
      }
      case Expr.Array array -> array.update(array.arrayBlock().map(
        left -> {
          var mCtx = MutableValue.create(ctx);
          var binds = resolve(left.binds(), mCtx);
          var generator = enter(mCtx.get()).apply(left.generator());
          return left.update(generator, binds, apply(left.bindName()), apply(left.pureName()));
        },
        right -> right.descent(this)
      ));
      case Expr.Unresolved(var pos, var name) -> switch (ctx.get(name)) {
        case GeneralizedVar generalized -> {
          if (!allowedGeneralizes.containsKey(generalized)) {
            if (options.allowGeneralized()) {
              // Ordered set semantics. Do not expect too many generalized vars.
              var owner = generalized.owner;
              assert owner != null : "Sainty check";
              allowedGeneralizes.put(generalized, owner.toExpr(false, generalized.toLocal()));
              addReference(owner);
            } else {
              ctx.reportAndThrow(new GeneralizedNotAvailableError(pos, generalized));
            }
          }
          yield new Expr.Ref(pos, allowedGeneralizes.get(generalized).ref());
        }
        case DefVar<?, ?> def -> {
          // RefExpr is referring to a serialized core which is already tycked.
          // Collecting tyck order for tycked terms is unnecessary, just skip.
          if (def.concrete == null) assert def.core != null;
          else if (def.concrete instanceof TyckUnit unit) addReference(unit);
          if (def.core instanceof PrimDef prim && PrimDef.ID.projSyntax(prim.id))
            ctx.reportAndThrow(new PrimResolveError.BadUsage(name.join(), pos));
          yield new Expr.Ref(pos, def);
        }
        case AnyVar var -> new Expr.Ref(pos, var);
      };
      default -> EndoExpr.super.apply(expr);
    };
  }

  private void addReference(@NotNull TyckUnit unit) {
    if (parentAdd != null) parentAdd.accept(unit);
    if (where.isEmpty()) throw new InternalException("where am I?");
    if (where.peek() == Where.Head) {
      reference.append(new TyckOrder.Head(unit));
      reference.append(new TyckOrder.Body(unit));
    } else {
      reference.append(new TyckOrder.Body(unit));
    }
  }

  public @NotNull Pattern.Clause apply(@NotNull Pattern.Clause clause) {
    var mCtx = MutableValue.create(ctx());
    var pats = clause.patterns.map(pa -> pa.descent(pat -> resolve(pat, mCtx)));
    return clause.update(pats, clause.expr.map(enter(mCtx.get())));
  }

  public static @NotNull Pattern resolve(@NotNull Pattern pattern, @NotNull MutableValue<Context> ctx) {
    return new EndoPattern() {
      @Override public @NotNull Pattern post(@NotNull Pattern pattern) {
        return switch (pattern) {
          case Pattern.Bind bind -> {
            var maybe = ctx.get().iterate(c -> switch (c.getUnqualifiedLocalMaybe(bind.bind().name(), bind.sourcePos())) {
              case DefVar<?, ?> def when def.core instanceof CtorDef || def.concrete instanceof TeleDecl.DataCtor
                || def.core instanceof PrimDef || def.concrete instanceof TeleDecl.PrimDecl -> def;
              case null, default -> null;
            });
            if (maybe != null) yield new Pattern.Ctor(bind, maybe);
            ctx.set(ctx.get().bind(bind.bind(), bind.sourcePos(), var -> false));
            yield bind;
          }
          case Pattern.Tuple tuple -> {
            ctx.set(bindAs(tuple.as(), ctx.get(), tuple.sourcePos()));
            yield tuple;
          }
          case Pattern.BinOpSeq seq -> {
            ctx.set(bindAs(seq.as(), ctx.get(), seq.sourcePos()));
            yield seq;
          }
          case Pattern.List list -> {
            ctx.set(bindAs(list.as(), ctx.get(), list.sourcePos()));
            yield list;
          }
          default -> EndoPattern.super.post(pattern);
        };
      }
    }.apply(pattern);
  }

  private static Context bindAs(LocalVar as, Context ctx, SourcePos sourcePos) {
    return as != null ? ctx.bind(as, sourcePos) : ctx;
  }

  public @NotNull Expr.Param resolve(@NotNull Expr.Param param, @NotNull MutableValue<Context> ctx) {
    var p = param.descent(enter(ctx.get()));
    ctx.set(ctx.get().bind(param.ref(), param.sourcePos()));
    return p;
  }

  public @NotNull ImmutableSeq<Expr.DoBind>
  resolve(@NotNull ImmutableSeq<Expr.DoBind> binds, @NotNull MutableValue<Context> ctx) {
    return binds.map(bind -> {
      var b = bind.descent(enter(ctx.get()));
      ctx.set(ctx.get().bind(bind.var(), bind.sourcePos()));
      return b;
    });
  }

  enum Where {
    Head, Body
  }

  /**
   * @param allowLevels true for signatures, false for bodies
   */
  public record Options(boolean allowLevels, boolean allowGeneralized) {
  }
}
