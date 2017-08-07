import scalafix._
import scalafix.syntax._
import scala.meta._

package object fix {

  private[fix] def rename(
      ctx: RewriteCtx,
      t: Term.Name,
      renames: Map[String, String])(implicit mirror: Mirror): Patch = {
    renames.collect {
      case (target, rename) if t.isSymbol(target) =>
        ctx.replaceTree(t, rename)
    }.asPatch
  }

  implicit class TermNameOps(t: Name) {
    def isSymbol(s: String)(implicit mirror: Mirror): Boolean =
      t.symbolOpt.exists(_.normalized.syntax == s)
  }

  implicit class OptionTermNameOps(t: Option[Name]) {
    def isSymbol(s: String)(implicit mirror: Mirror): Boolean =
      t.flatMap(_.symbolOpt).exists(_.normalized.syntax == s)
  }

}
