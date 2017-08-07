package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1614
case class SimplifyEitherTLift(mirror: Mirror) extends SemanticRewrite(mirror) {

  private[this] val leftSymbol = "_root_.cats.data.EitherTFunctions.left."
  private[this] val rightSymbol = "_root_.cats.data.EitherTFunctions.right."

  private[this] def removeWithLeadingComma(ctx: RewriteCtx, t: Tree): Patch =
    (for {
      leadingComma <- ctx.tokenList.leading(t.tokens.head).find(_.syntax == ",")
    } yield {
      val leadingSpaces = ctx.tokenList.slice(leadingComma, t.tokens.head)
      ctx.removeToken(leadingComma) +
        leadingSpaces.map(ctx.removeToken).asPatch +
        ctx.removeTokens(t.tokens)
    }).asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case Term.ApplyType(Term.Select(_, name), Seq(f, a, b))
          if name.isSymbol(leftSymbol) =>
        ctx.replaceTree(name, "leftT") + removeWithLeadingComma(ctx, a)
      case Term.ApplyType(Term.Select(_, name), Seq(f, a, b))
          if name.isSymbol(rightSymbol) =>
        ctx.replaceTree(name, "pure") + removeWithLeadingComma(ctx, b)
    }.asPatch
  }

}
