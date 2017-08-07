package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1709
case class RenameFreeSuspend(mirror: Mirror) extends SemanticRewrite(mirror) {

  private[this] val renames = Map(
    "_root_.cats.free.Free.suspend." -> "defer",
    "_root_.cats.free.TrampolineFunctions.suspend." -> "defer"
  )

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

}
