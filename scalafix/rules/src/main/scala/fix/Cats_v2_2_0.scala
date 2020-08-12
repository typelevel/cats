package fix
package v2_2_0

import scalafix.v0._
import scalafix.syntax._
import scala.meta._
import scala.meta.contrib._

// ref: https://github.com/typelevel/cats/issues/3563
case class RemoveInstanceImports(index: SemanticdbIndex)
  extends SemanticRule(index, "RemoveInstanceImports") {

  override def fix(ctx: RuleCtx): Patch = {

    println(ctx.tree.structureLabeled)

    ctx.tree.collect {
      case i @ q"import cats.instances.option._" => ctx.removeTokens(i.tokens)
    }.asPatch
  }
}
