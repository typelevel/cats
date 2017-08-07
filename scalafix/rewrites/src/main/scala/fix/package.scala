import scalafix._
import scalafix.syntax._
import scala.meta._

package object fix {

  private[fix] def rename(
      ctx: RewriteCtx,
      t: Term.Name,
      renames: Map[String, String])(implicit mirror: Mirror): Patch = {
    renames.collect {
      case (target, rename) if t.symbolOpt.exists(_.normalized.syntax == target) =>
        ctx.replaceTree(t, rename)
    }.asPatch
  }

}
