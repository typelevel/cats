package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1611
case class RenameReducibleMethods(mirror: Mirror)
    extends SemanticRewrite(mirror) {

  private[this] val renames = Map(
    "_root_.cats.Reducible.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.Ops.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.Ops.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.sequence1_." -> "nonEmptySequence_",
    "_root_.cats.Reducible.Ops.sequence1_." -> "nonEmptySequence_"
  )

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

}
