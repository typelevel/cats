package fix
package v1_0_0

import scalafix._
import scalafix.syntax._
import scalafix.util.SymbolMatcher
import scala.meta._
import scala.meta.contrib._

// ref: https://github.com/typelevel/cats/pull/1745
case class RemoveCartesianBuilder(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveCartesianBuilder") {

  private[this] val cartesianBuilders = SymbolMatcher.normalized(
    Symbol("_root_.cats.syntax.CartesianOps.`|@|`.") ::
      (1 to 22).toList.map(arity =>
      Symbol(
        s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.`|@|`.")): _*
  )

  private[this] val partialApplies = SymbolMatcher.normalized(
    Symbol(s"_root_.cats.syntax.CartesianOps.`*>`."),
    Symbol(s"_root_.cats.syntax.CartesianOps.`<*`.")
  )

  private[this] val renames: Map[String, String] = (1 to 22).flatMap { arity =>
    Seq(
      s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.map." -> "mapN",
      s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.imap." -> "imapN",
      s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.contramap." -> "contramapN"
    )
  }.toMap

  private[this] val cartesianOps =
    SymbolMatcher.normalized(renames.keys.map(Symbol.apply).toSeq: _*)

  private[this] def replaceOpWithComma(ctx: RuleCtx, op: Term.Name): Patch =
    // replace |@| with ,
    ctx.replaceTree(op, ",") ++
      // remove the space before |@|
      ctx.tokenList
        .leading(op.tokens.head)
        .takeWhile(_.is[Whitespace])
        .map(ctx.removeToken)

  private[this] def wrapInParensIfNeeded(ctx: RuleCtx, t: Term): Patch = {
    for {
      head <- t.tokens.headOption
      if !head.is[Token.LeftParen]
      last <- t.tokens.lastOption
      if !last.is[Token.RightParen]
    } yield
      ctx.addLeft(head, "(") +
        ctx.addRight(last, ")")
  }.asPatch

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case Term.ApplyInfix(_, cartesianBuilders(op: Term.Name), _, _) =>
        replaceOpWithComma(ctx, op)
      case Term.ApplyInfix(lhs, cartesianOps(_), _, _) =>
        wrapInParensIfNeeded(ctx, lhs)
      case t @ q"import cats.syntax.cartesian._" =>
        val usesPartialApplies = ctx.tree.exists {
          case partialApplies(_: Term.Name) => true
          case _ => false
        }
        if (usesPartialApplies) {
          ctx.addRight(t.tokens.last, "\n  import cats.syntax.apply._")
        } else {
          ctx.replaceTree(t, "import cats.syntax.apply._")
        }
    }.asPatch + ctx.replaceSymbols(renames.toSeq: _*)
  }
}

// ref: https://github.com/typelevel/cats/issues/1850
case class ContraMapToLMap(index: SemanticdbIndex)
  extends SemanticRule(index, "UseLMapInsteadOfContraMap") {

  //val contraMap = Symbol("_root_.cats.functor.Contravariant.Ops.contramap")


  override def fix(ctx: RuleCtx): Patch = {

    val contraMatcher = SymbolMatcher.normalized(Symbol("_root_.cats.functor.Contravariant.Ops.`contramap`."))

    val unApplyName = "catsUnapply2left"

    ctx.tree.collect {
      case Term.Apply(fun, _) =>
        if (contraMatcher.matches(fun) &&
          fun.children.headOption.flatMap(index.denotation).exists{ x => println(x.name == unApplyName); x.name == unApplyName }) {
          fun.children.find(contraMatcher.matches).map(tree => ctx.replaceTree(tree, "lmap")).getOrElse(Patch.empty)
        } else {
          Patch.empty
        }
      case _ => Patch.empty

    }.asPatch
  }
}

// ref: https://github.com/typelevel/cats/pull/1583
case class RemoveUnapply(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveUnapply") {

  override def fix(ctx: RuleCtx): Patch = ctx.replaceSymbols(
    "_root_.cats.Traverse.Ops.traverseU." -> "traverse",
    "_root_.cats.Foldable.Ops.traverseU_." -> "traverse_",
    "_root_.cats.Foldable.traverseU_." -> "traverse_",
    "_root_.cats.Traverse.Ops.sequenceU." -> "sequence",
    "_root_.cats.Foldable.Ops.sequenceU_." -> "sequence_",
    "_root_.cats.Foldable.sequenceU_." -> "sequence_",
    "_root_.cats.data.Func.appFuncU." -> "appFunc",
    "_root_.cats.free.FreeT.liftTU." -> "liftT"
  )
}

// ref: https://github.com/typelevel/cats/pull/1709
case class RenameFreeSuspend(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameFreeSuspend") {

  override def fix(ctx: RuleCtx): Patch = ctx.replaceSymbols(
    "_root_.cats.free.Free.suspend." -> "defer",
    "_root_.cats.free.TrampolineFunctions.suspend." -> "defer"
  )

}

// ref: https://github.com/typelevel/cats/pull/1611
case class RenameReducibleMethods(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameReducibleMethods") {

  override def fix(ctx: RuleCtx): Patch = ctx.replaceSymbols(
    "_root_.cats.Reducible.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.Ops.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.Ops.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.sequence1_." -> "nonEmptySequence_",
    "_root_.cats.Reducible.Ops.sequence1_." -> "nonEmptySequence_"
  )

}

// ref: https://github.com/typelevel/cats/pull/1614
case class SimplifyEitherTLift(index: SemanticdbIndex)
    extends SemanticRule(index, "SimplifyEitherTLift") {

  private[this] val leftSymbol = SymbolMatcher.normalized(
    Symbol("_root_.cats.data.EitherTFunctions.left.")
  )
  private[this] val rightSymbol = SymbolMatcher.normalized(
    Symbol("_root_.cats.data.EitherTFunctions.right.")
  )

  private[this] def removeWithLeadingComma(ctx: RuleCtx, t: Tree): Patch =
    (for {
      leadingComma <- ctx.tokenList
        .leading(t.tokens.head)
        .find(_.is[Token.Comma])
    } yield {
      val leadingSpaces = ctx.tokenList.slice(leadingComma, t.tokens.head)
      ctx.removeToken(leadingComma) ++
        leadingSpaces.map(ctx.removeToken) +
        ctx.removeTokens(t.tokens)
    }).asPatch

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case Term.ApplyType(Term.Select(_, leftSymbol(name)), Seq(_, a, _)) =>
        ctx.replaceTree(name, "leftT") + removeWithLeadingComma(ctx, a)
      case Term.ApplyType(Term.Select(_, rightSymbol(name)), Seq(_, _, b)) =>
        ctx.replaceTree(name, "pure") + removeWithLeadingComma(ctx, b)
    }.asPatch
  }

}

// ref: https://github.com/typelevel/cats/pull/1589
//      https://github.com/typelevel/cats/pull/1596
case class RenameInjectProdAndCoproduct(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameInjectProdAndCoproduct") {

  override def fix(ctx: RuleCtx): Patch = ctx.replaceSymbols(
    "_root_.cats.free.Inject." -> "_root_.cats.InjectK.",
    "_root_.cats.data.Prod." -> "_root_.cats.data.Tuple2K.",
    "_root_.cats.data.Coproduct." -> "_root_.cats.data.EitherK."
  )

}

// ref: https://github.com/typelevel/cats/pull/1487
case class RenameTupleApplySyntax(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameTupleApplySyntax") {

  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      (1 to 22).flatMap { arity =>
        Seq(
          s"_root_.cats.syntax.Tuple${arity}CartesianOps.map$arity." -> "mapN",
          s"_root_.cats.syntax.Tuple${arity}CartesianOps.contramap$arity." -> "contramapN",
          s"_root_.cats.syntax.Tuple${arity}CartesianOps.imap$arity." -> "imapN"
        )
      }: _*
    ) ++
      ctx.tree.collect {
        case t @ q"import cats.syntax.tuple._" =>
          ctx.replaceTree(t, "import cats.syntax.apply._")
      }
  }
}

// ref: https://github.com/typelevel/cats/pull/1766
case class RemoveSplit(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveSplit") {

  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      "_root_.cats.arrow.Split." -> "_root_.cats.arrow.Arrow."
    ) + ctx.tree.collect {
      case t @ q"import cats.syntax.split._" =>
        ctx.replaceTree(t, "import cats.syntax.arrow._")
    }.asPatch
  }

}
