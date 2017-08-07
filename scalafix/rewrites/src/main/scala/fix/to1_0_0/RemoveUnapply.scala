package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1583
case class RemoveUnapply(mirror: Mirror) extends SemanticRewrite(mirror) {

  private[this] val renames = Map(
    "_root_.cats.Traverse.Ops.traverseU." -> "traverse",
    "_root_.cats.Foldable.Ops.traverseU_." -> "traverse_",
    "_root_.cats.Foldable.traverseU_." -> "traverse_",
    "_root_.cats.Traverse.Ops.sequenceU." -> "sequence",
    "_root_.cats.Foldable.Ops.sequenceU_." -> "sequence_",
    "_root_.cats.Foldable.sequenceU_." -> "sequence_",
    "_root_.cats.data.Func.appFuncU." -> "appFunc",
    "_root_.cats.free.FreeT.liftTU." -> "liftT"
  )

  private[this] def importeeName(importee: Importee): Option[Name] =
    importee match {
      case Importee.Name(name) => Some(name)
      case Importee.Rename(name, _) => Some(name)
      case _ => None
    }

  private[this] def removeImportee(
      ctx: RewriteCtx,
      importee: Importee,
      fixes: Map[String, String]): Patch =
    fixes.collect {
      case (target, _) if importeeName(importee).isSymbol(target) =>
        ctx.removeImportee(importee)
    }.asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
      case t: Importee.Name => removeImportee(ctx, t, renames)
    }.asPatch
  }
}
