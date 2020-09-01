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
    // e.g. "import cats.instances.int._" or "import cats.instances.all._"
    case i @ Import(Importer(Select(Select(Name("cats"), Name("instances")), x), _) :: _) =>
      removeImportLine(ctx)(i)

    // "import cats.implicits._"
    case i @ Import(Importer(Select(Name("cats"), Name("implicits")), _) :: _) =>
      val boundary = findLexicalBoundary(i)

      // Find all synthetics between the import statement and the end of the lexical boundary
      val lexicalStart = i.pos.end
      val lexicalEnd = boundary.pos.end
      try {
        val relevantSynthetics =
          ctx.index.synthetics.filter(x => x.position.start >= lexicalStart && x.position.end <= lexicalEnd)

        val usesSyntax = relevantSynthetics.exists(containsCatsSyntax)
        if (usesSyntax) {
          // the import is being used to enable an extension method,
          // so replace it with "import cats.syntax.all._"
          ctx.replaceTree(i, "import cats.syntax.all._")
        } else {
          // the import is only used to import instances,
          // so it's safe to remove
          removeImportLine(ctx)(i)
        }
      } catch {
        case e: scalafix.v1.MissingSymbolException =>
          // see https://github.com/typelevel/cats/pull/3566#issuecomment-684007028
          // and https://github.com/scalacenter/scalafix/issues/1123
          println(s"Skipping rewrite of 'import cats.implicits._' in file ${ctx.input.label} because we ran into a Scalafix bug. $e")
          e.printStackTrace()
          Patch.empty
      }
  }.asPatch

  private def removeImportLine(ctx: RuleCtx)(i: Import): Patch =
    ctx.removeTokens(i.tokens) + removeWhitespaceAndNewlineBefore(ctx)(i.tokens.start)

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

}
