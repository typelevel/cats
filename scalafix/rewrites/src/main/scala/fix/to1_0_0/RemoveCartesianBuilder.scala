package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1745
case class RemoveCartesianBuilder(mirror: Mirror)
    extends SemanticRewrite(mirror) {

  private[this] val cartesianImport =
    "cats."

  private[this] val cartesianBuilders =
    (1 to 22)
      .map(arity =>
        s"_root_.cats.syntax.CartesianBuilder#CartesianBuilder$arity.`|@|`.")
      .toSet +
      "_root_.cats.syntax.CartesianOps.`|@|`."

  private[this] val cartesianFixes: Map[String, String] =
    (1 to 22)
      .map { arity =>
        Seq(
          s"_root_.cats.syntax.CartesianBuilder#CartesianBuilder$arity.map." -> "mapN",
          s"_root_.cats.syntax.CartesianBuilder#CartesianBuilder$arity.imap." -> "imapN",
          s"_root_.cats.syntax.CartesianBuilder#CartesianBuilder$arity.contramap." -> "contramapN"
        )
      }
      .flatten
      .toMap

  private[this] def replace(
      ctx: RewriteCtx,
      t: Term.Name,
      fixes: Map[String, String]): Patch = {
    fixes.collect {
      case (target, fix) if t.symbolOpt.exists(_.normalized.syntax == target) =>
        ctx.replaceTree(t, fix)
    }.asPatch
  }

  // Hackish to work around duplicate fixes due to recursion
  val alreadyFixedOps = collection.mutable.Set.empty[Term.Name]
  private[this] def replaceOpWithComma(ctx: RewriteCtx, op: Term.Name): Patch =
    if (op.symbolOpt.exists(s =>
        cartesianBuilders.contains(s.normalized.syntax)) && !alreadyFixedOps
        .contains(op)) {
      alreadyFixedOps += op
      // remove the space before |@|
      ctx.removeToken(ctx.tokenList.prev(op.tokens.head)) +
        // replace |@| with ,
        ctx.replaceTree(op, ",")
    } else {
      Patch.empty
    }

  private[this] def removeCartesianBuilderOp(
      ctx: RewriteCtx,
      applyInfix: Term.ApplyInfix): Patch = {
    applyInfix match {
      case Term.ApplyInfix(lhs: Term.ApplyInfix, op, _, _) =>
        removeCartesianBuilderOp(ctx, lhs) + replaceOpWithComma(ctx, op)
      case Term.ApplyInfix(_, op, _, _) =>
        replaceOpWithComma(ctx, op)
    }
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.ApplyInfix => removeCartesianBuilderOp(ctx, t)
      case Term.Select(_, fun) => replace(ctx, fun, cartesianFixes)
      case t @ q"import cats.syntax.cartesian._" =>
        ctx.replaceTree(t, "import cats.syntax.apply._")
    }.asPatch
  }
}
