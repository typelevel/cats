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
      // e.g. "import cats.instances.int._"
      case i @ Import(Importer(Select(Select(Name("cats"), Name("instances")), x), _) :: _) if definitelySafeToRemove(x.value) =>
        ctx.removeTokens(i.tokens) + removeWhitespaceAndNewlineBefore(ctx)(i.tokens.start)
    }.asPatch
  }

  private def removeWhitespaceAndNewlineBefore(ctx: RuleCtx)(index: Int): Patch = {
    val whitespaceAndNewlines = ctx.tokens.take(index).takeRightWhile(t =>
      t.is[Token.Space] ||
      t.is[Token.Tab] ||
      t.is[Token.LF]
    )
    ctx.removeTokens(whitespaceAndNewlines)
  }

  /*
   * Any "import cats.instances.foo._ is definitely safe to remove,
   * apart from the following:
   *
   * - import cats.instances.future._
   *
   *   This is still needed, because the Future instances have not
   *   been moved to implicit scope in Cats 2.2.0.
   *   See https://github.com/typelevel/cats/issues/3536
   *
   * - import cats.instances.all._
   *
   *   This might be used to import Future instances. Further checking
   *   of the surrounding code is needed to see whether that is the case.
   */
  private val definitelySafeToRemove = Set(
    "bigDecimal",
    "bigInt",
    "bitSet",
    "boolean",
    "byte",
    "char",
    "double",
    "duration",
    "either",
    "eq",
    "equiv",
    "finiteDuration",
    "float",
    "function",
    "int",
    "invariant",
    "lazyList",
    "list",
    "long",
    "map",
    "option",
    "order",
    "ordering",
    "parallel",
    "partialOrder",
    "partialOrdering",
    "queue",
    "set",
    "short",
    "sortedMap",
    "sortedSet",
    "string",
    "symbol",
    "tailRec",
    "try_",
    "tuple",
    "unit",
    "uuid",
    "vector",
  )

}
