package fix
package v1_0_0

import scalafix._
import scalafix.util.SymbolMatcher
import scalafix.syntax._
import scala.meta._

object Utils {

  private[fix] def rename(
      ctx: RuleCtx,
      t: Term.Name,
      renames: Map[Symbol, String])(implicit index: SemanticdbIndex): Patch = {
    renames.collect {
      case (target, rename) if SymbolMatcher.normalized(target).matches(t) =>
        ctx.replaceTree(t, rename)
    }.asPatch
  }

}
import Utils._

// ref: https://github.com/typelevel/cats/pull/1745
case class RemoveCartesianBuilder(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveCartesianBuilder") {

  private[this] val cartesianBuilders = SymbolMatcher.normalized(
    Symbol("_root_.cats.syntax.CartesianOps.`|@|`.") ::
      (1 to 22).toList.map(arity =>
      Symbol(
        s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.`|@|`.")): _*)

  private[this] val partialApplies = SymbolMatcher.normalized(
    Symbol(s"_root_.cats.syntax.CartesianOps.`*>`."),
    Symbol(s"_root_.cats.syntax.CartesianOps.`<*`.")
  )

  private[this] val renames: Map[Symbol, String] =
    (1 to 22)
      .map { arity =>
        Seq(
          Symbol(
            s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.map.") -> "mapN",
          Symbol(
            s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.imap.") -> "imapN",
          Symbol(
            s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.contramap.") -> "contramapN"
        )
      }
      .flatten
      .toMap

  private[this] val cartesianOps =
    SymbolMatcher.normalized(renames.keys.toSeq: _*)

  // Hackish way to work around duplicate fixes due to recursion
  val alreadyFixedOps = collection.mutable.Set.empty[Term.Name]
  private[this] def replaceOpWithComma(ctx: RuleCtx, op: Term.Name): Patch =
    if (op.matches(cartesianBuilders) && !alreadyFixedOps.contains(op)) {
      alreadyFixedOps += op
      // remove the space before |@|
      ctx.removeToken(ctx.tokenList.prev(op.tokens.head)) +
        // replace |@| with ,
        ctx.replaceTree(op, ",")
    } else {
      Patch.empty
    }

  private[this] def removeCartesianBuilderOp(
      ctx: RuleCtx,
      applyInfix: Term.ApplyInfix): Patch = {
    applyInfix match {
      case Term.ApplyInfix(lhs: Term.ApplyInfix, op, _, _) =>
        removeCartesianBuilderOp(ctx, lhs) + replaceOpWithComma(ctx, op)
      case Term.ApplyInfix(_, op, _, _) =>
        replaceOpWithComma(ctx, op)
    }
  }

  private[this] def wrapInParensIfNeeded(ctx: RuleCtx, t: Term): Patch = {
    if (t.tokens.head.is[Token.LeftParen] && t.tokens.last
        .is[Token.RightParen]) {
      Patch.empty
    } else {
      ctx.addLeft(t.tokens.head, "(") + ctx.addRight(t.tokens.last, ")")
    }
  }

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t: Term.ApplyInfix if t.op.matches(cartesianBuilders) =>
        removeCartesianBuilderOp(ctx, t)
      case t: Term.ApplyInfix if t.op.matches(cartesianOps) =>
        wrapInParensIfNeeded(ctx, t.lhs)
      case t: Term.Name => rename(ctx, t, renames)
      case t @ q"import cats.syntax.cartesian._" =>
        val usesPartialApplies = ctx.tree.collect {
          case partialApplies(t: Term.Name) => ()
        }.length > 0
        if (usesPartialApplies) {
          ctx.addRight(t.tokens.last, "\n  import cats.syntax.apply._")
        } else {
          ctx.replaceTree(t, "import cats.syntax.apply._")
        }
    }.asPatch
  }
}

// ref: https://github.com/typelevel/cats/pull/1583
case class RemoveUnapply(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveUnapply") {

  private[this] val renames = Map(
    Symbol("_root_.cats.Traverse.Ops.traverseU.") -> "traverse",
    Symbol("_root_.cats.Foldable.Ops.traverseU_.") -> "traverse_",
    Symbol("_root_.cats.Foldable.traverseU_.") -> "traverse_",
    Symbol("_root_.cats.Traverse.Ops.sequenceU.") -> "sequence",
    Symbol("_root_.cats.Foldable.Ops.sequenceU_.") -> "sequence_",
    Symbol("_root_.cats.Foldable.sequenceU_.") -> "sequence_",
    Symbol("_root_.cats.data.Func.appFuncU.") -> "appFunc",
    Symbol("_root_.cats.free.FreeT.liftTU.") -> "liftT"
  )

  private[this] def importeeName(importee: Importee): Option[Name] =
    importee match {
      case Importee.Name(name) => Some(name)
      case Importee.Rename(name, _) => Some(name)
      case _ => None
    }

  private[this] def removeImportee(
      ctx: RuleCtx,
      importee: Importee,
      fixes: Map[Symbol, String]): Patch = {
    val importsToRemove = SymbolMatcher.normalized(fixes.keys.toSeq: _*)
    importeeName(importee).collect {
      case importsToRemove(n) => ctx.removeImportee(importee)
    }.asPatch
  }

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
      case t: Importee.Name => removeImportee(ctx, t, renames)
    }.asPatch
  }
}

// ref: https://github.com/typelevel/cats/pull/1709
case class RenameFreeSuspend(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameFreeSuspend") {

  private[this] val renames = Map(
    Symbol("_root_.cats.free.Free.suspend.") -> "defer",
    Symbol("_root_.cats.free.TrampolineFunctions.suspend.") -> "defer"
  )

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

}

// ref: https://github.com/typelevel/cats/pull/1611
case class RenameReducibleMethods(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameReducibleMethods") {

  private[this] val renames = Map(
    Symbol("_root_.cats.Reducible.traverse1_.") -> "nonEmptyTraverse_",
    Symbol("_root_.cats.Reducible.Ops.traverse1_.") -> "nonEmptyTraverse_",
    Symbol("_root_.cats.Reducible.intercalate1.") -> "nonEmptyIntercalate",
    Symbol("_root_.cats.Reducible.Ops.intercalate1.") -> "nonEmptyIntercalate",
    Symbol("_root_.cats.Reducible.sequence1_.") -> "nonEmptySequence_",
    Symbol("_root_.cats.Reducible.Ops.sequence1_.") -> "nonEmptySequence_"
  )

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

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
      leadingComma <- ctx.tokenList.leading(t.tokens.head).find(_.syntax == ",")
    } yield {
      val leadingSpaces = ctx.tokenList.slice(leadingComma, t.tokens.head)
      ctx.removeToken(leadingComma) +
        leadingSpaces.map(ctx.removeToken).asPatch +
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

  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      "_root_.cats.free.Inject." -> "_root_.cats.InjectK.",
      "_root_.cats.data.Prod." -> "_root_.cats.data.Tuple2K.",
      "_root_.cats.data.Coproduct." -> "_root_.cats.data.EitherK."
    )
  }

}

// ref: https://github.com/typelevel/cats/pull/1487
case class RenameTupleApplySyntax(index: SemanticdbIndex)
    extends SemanticRule(index, "RenameTupleApplySyntax") {

  private[this] val renames: Map[Symbol, String] =
    (1 to 22)
      .map { arity =>
        Seq(
          Symbol(s"_root_.cats.syntax.Tuple${arity}CartesianOps.map$arity.") -> "mapN",
          Symbol(
            s"_root_.cats.syntax.Tuple${arity}CartesianOps.contramap$arity.") -> "contramapN",
          Symbol(s"_root_.cats.syntax.Tuple${arity}CartesianOps.imap$arity.") -> "imapN"
        )
      }
      .flatten
      .toMap

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
      case t @ q"import cats.syntax.tuple._" =>
        ctx.replaceTree(t, "import cats.syntax.apply._")
    }.asPatch
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
