package fix
package to1_0_0

import scalafix._
import scala.meta._

case class All(mirror: Mirror) extends SemanticRewrite(mirror) {

  def rewrite(ctx: RewriteCtx): Patch =
    Seq(
      RemoveUnapply(mirror),
      RemoveCartesianBuilder(mirror),
      RenameFreeSuspend(mirror)
    ).reduce(Rewrite.merge).rewrite(ctx)
}
