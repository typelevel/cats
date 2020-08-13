package fix
package v2_2_0

import scalafix.v0._
import scalafix.syntax._
import scala.meta._
import scala.meta.contrib._
import scala.meta.Term.Apply

// ref: https://github.com/typelevel/cats/issues/3563
case class RemoveInstanceImports(index: SemanticdbIndex)
  extends SemanticRule(index, "RemoveInstanceImports") {

  override def fix(ctx: RuleCtx): Patch = ctx.tree.collect {
    // e.g. "import cats.instances.int._"
    case i @ Import(Importer(Select(Select(Name("cats"), Name("instances")), x), _) :: _) if definitelySafeToRemove(x.value) =>
      removeImportLine(ctx)(i)

    // "import cats.instances.all._"
    case i @ Import(Importer(Select(Select(Name("cats"), Name("instances")), Name("all")), _) :: _) =>
      val boundary = findLexicalBoundary(i)

      // Find all synthetics between the import statement and the end of the lexical boundary
      val lexicalStart = i.pos.end
      val lexicalEnd = boundary.pos.end
      val relevantSynthetics =
        ctx.index.synthetics.filter(x => x.position.start >= lexicalStart && x.position.end <= lexicalEnd)

      val usesFutureInstance = relevantSynthetics.exists(containsCatsFutureInstance)
      if (usesFutureInstance) {
        // the import is (probably) being used to import instances for Future,
        // so we can't remove it
        Patch.empty
      } else {
        // no usage of Future instances, so it's safe to remove the import
        removeImportLine(ctx)(i)
      }

    // "import cats.implicits._"
    case i @ Import(Importer(Select(Name("cats"), Name("implicits")), _) :: _) =>
      val boundary = findLexicalBoundary(i)

      // Find all synthetics between the import statement and the end of the lexical boundary
      val lexicalStart = i.pos.end
      val lexicalEnd = boundary.pos.end
      val relevantSynthetics =
        ctx.index.synthetics.filter(x => x.position.start >= lexicalStart && x.position.end <= lexicalEnd)

      val usesFutureInstance = relevantSynthetics.exists(containsCatsFutureInstance)
      val usesSyntax = relevantSynthetics.exists(containsCatsSyntax)
      if (usesFutureInstance || usesSyntax) {
        // the import is being used either to import a Future instance
        // or to enable an extension method, so we can't remove it
        Patch.empty
      } else {
        // safe to remove
        removeImportLine(ctx)(i)
      }
  }.asPatch

  private def removeImportLine(ctx: RuleCtx)(i: Import): Patch =
    ctx.removeTokens(i.tokens) + removeWhitespaceAndNewlineBefore(ctx)(i.tokens.start)

  private def containsCatsFutureInstance(synthetic: Synthetic) =
    synthetic.names.exists(x => isCatsFutureInstance(x.symbol))

  private def isCatsFutureInstance(symbol: Symbol) =
    symbol.syntax.contains("cats") && symbol.syntax.contains("Future")

  private def containsCatsSyntax(synthetic: Synthetic) =
    synthetic.names.exists(x => isCatsSyntax(x.symbol))

  private def isCatsSyntax(symbol: Symbol) =
    symbol.syntax.contains("cats") && symbol.syntax.contains("syntax")

  private def findLexicalBoundary(t: Tree): Tree = {
    t.parent match {
      case Some(b: Term.Block) => b
      case Some(t: Template) => t
      case Some(parent) => findLexicalBoundary(parent)
      case None => t
    }
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
