package fix
package to1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta.{Symbol => _, _}

// ref: https://github.com/typelevel/cats/pull/1583
case class RemoveUnapply(mirror: Mirror) extends SemanticRewrite(mirror) {
  private[this] val fixes = Map(
    "_root_.cats.Traverse.Ops.traverseU." -> "traverse",
    "_root_.cats.Foldable.Ops.traverseU_." -> "traverse_",
    "_root_.cats.Foldable.traverseU_." -> "traverse_",
    "_root_.cats.Traverse.Ops.sequenceU." -> "sequence",
    "_root_.cats.Foldable.Ops.sequenceU_." -> "sequence_",
    "_root_.cats.Foldable.sequenceU_." -> "sequence_",
    "_root_.cats.data.Func.appFuncU." -> "appFunc",
    "_root_.cats.free.FreeT.liftTU." -> "liftT"
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
      case (target, _)
          if importeeName(importee)
            .flatMap(_.symbolOpt)
            .exists(_.normalized.syntax == target) =>
        ctx.removeImportee(importee)
    }.asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case Term.Select(_, fun) => replace(ctx, fun, fixes)
      case Term.Apply(fun: Term.Name, _) => replace(ctx, fun, fixes)
      case t: Importee.Name => removeImportee(ctx, t, fixes)
    }.asPatch
  }
}
