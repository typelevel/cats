package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1709
case class RenameFreeSuspend(mirror: Mirror) extends SemanticRewrite(mirror) {
  private[this] val fixes = Map(
    "_root_.cats.free.Free.suspend." -> "defer",
    "_root_.cats.free.TrampolineFunctions.suspend." -> "defer"
  )

  private[this] def replace(
      ctx: RewriteCtx,
      t: Term.Name,
      fixes: Map[String, String]): Patch = {
    fixes.collect {
      case (target, fix) if t.symbolOpt.exists(_.normalized.syntax == target) =>
        ctx.replaceTree(t, fix)
    }.asPatch
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => replace(ctx, t, fixes)
    }.asPatch
  }
}
