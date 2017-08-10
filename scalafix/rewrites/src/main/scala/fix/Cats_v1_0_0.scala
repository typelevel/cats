package fix
package v1_0_0

import scalafix._
import scalafix.syntax._
import scala.meta._

object Utils {

  private[fix] def rename(
      ctx: RewriteCtx,
      t: Term.Name,
      renames: Map[String, String])(implicit semanticCtx: SemanticCtx): Patch = {
    renames.collect {
      case (target, rename) if t.isSymbol(target) =>
        ctx.replaceTree(t, rename)
    }.asPatch
  }

  implicit class TermNameOps(t: Name) {
    def isSymbol(s: String)(implicit semanticCtx: SemanticCtx): Boolean =
      t.symbol.exists(_.normalized.syntax == s)

    def isOneOfSymbols(symbols: Set[String])(implicit semanticCtx: SemanticCtx): Boolean =
      t.symbol.exists(s => symbols.contains(s.normalized.syntax))
  }

  implicit class OptionTermNameOps(t: Option[Name]) {
    def isSymbol(s: String)(implicit semanticCtx: SemanticCtx): Boolean =
      t.flatMap(_.symbol).exists(_.normalized.syntax == s)
  }

}
import Utils._

// ref: https://github.com/typelevel/cats/pull/1745
case class RemoveCartesianBuilder(semanticCtx: SemanticCtx)
    extends SemanticRewrite(semanticCtx) {

  private[this] val cartesianBuilders =
    (1 to 22)
      .map(arity =>
        s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.`|@|`.")
      .toSet +
      "_root_.cats.syntax.CartesianOps.`|@|`."

  private[this] val partialApplies = Set(
    s"_root_.cats.syntax.CartesianOps.`*>`.",
    s"_root_.cats.syntax.CartesianOps.`<*`."
  )

  private[this] val renames: Map[String, String] =
    (1 to 22)
      .map { arity =>
        Seq(
          s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.map." -> "mapN",
          s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.imap." -> "imapN",
          s"_root_.cats.syntax.CartesianBuilder.CartesianBuilder$arity.contramap." -> "contramapN"
        )
      }
      .flatten
      .toMap

  // Hackish way to work around duplicate fixes due to recursion
  val alreadyFixedOps = collection.mutable.Set.empty[Term.Name]
  private[this] def replaceOpWithComma(ctx: RewriteCtx, op: Term.Name): Patch =
    if (op.isOneOfSymbols(cartesianBuilders) && !alreadyFixedOps.contains(op)) {
      alreadyFixedOps += op
      // remove the space before |@|
      ctx.removeToken(ctx.tokenList.prev(op.tokens.head)) +
        // replace |@| with ,
        ctx.replaceTree(op, ",")
    } else {
      Patch.empty
    }

  private[this] def removeCartesianBuilderOp(
      ctx: RewriteCtx,
      applyInfix: Term.ApplyInfix): Patch = {
    applyInfix match {
      case Term.ApplyInfix(lhs: Term.ApplyInfix, op, _, _) =>
        removeCartesianBuilderOp(ctx, lhs) + replaceOpWithComma(ctx, op)
      case Term.ApplyInfix(_, op, _, _) =>
        replaceOpWithComma(ctx, op)
    }
  }

  private[this] def wrapInParensIfNeeded(ctx: RewriteCtx, t: Term): Patch = {
    if (t.tokens.head.is[Token.LeftParen] && t.tokens.last.is[Token.RightParen]) {
      Patch.empty
    } else {
      ctx.addLeft(t.tokens.head, "(") + ctx.addRight(t.tokens.last, ")")
    }
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.ApplyInfix if t.op.isOneOfSymbols(cartesianBuilders) =>
        removeCartesianBuilderOp(ctx, t)
      case t: Term.ApplyInfix if t.op.isOneOfSymbols(renames.keys.toSet) =>
        wrapInParensIfNeeded(ctx, t.lhs)
      case t: Term.Name => rename(ctx, t, renames)
      case t @ q"import cats.syntax.cartesian._" =>
        val usesPartialApplies = ctx.tree.collect {
           case t: Term.Name if t.isOneOfSymbols(partialApplies) => ()
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
case class RemoveUnapply(semanticCtx: SemanticCtx) extends SemanticRewrite(semanticCtx) {

  private[this] val renames = Map(
    "_root_.cats.Traverse.Ops.traverseU." -> "traverse",
    "_root_.cats.Foldable.Ops.traverseU_." -> "traverse_",
    "_root_.cats.Foldable.traverseU_." -> "traverse_",
    "_root_.cats.Traverse.Ops.sequenceU." -> "sequence",
    "_root_.cats.Foldable.Ops.sequenceU_." -> "sequence_",
    "_root_.cats.Foldable.sequenceU_." -> "sequence_",
    "_root_.cats.data.Func.appFuncU." -> "appFunc",
    "_root_.cats.free.FreeT.liftTU." -> "liftT"
  )

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
      case (target, _) if importeeName(importee).isSymbol(target) =>
        ctx.removeImportee(importee)
    }.asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
      case t: Importee.Name => removeImportee(ctx, t, renames)
    }.asPatch
  }
}

// ref: https://github.com/typelevel/cats/pull/1709
case class RenameFreeSuspend(semanticCtx: SemanticCtx) extends SemanticRewrite(semanticCtx) {

  private[this] val renames = Map(
    "_root_.cats.free.Free.suspend." -> "defer",
    "_root_.cats.free.TrampolineFunctions.suspend." -> "defer"
  )

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

}

// ref: https://github.com/typelevel/cats/pull/1611
case class RenameReducibleMethods(semanticCtx: SemanticCtx)
    extends SemanticRewrite(semanticCtx) {

  private[this] val renames = Map(
    "_root_.cats.Reducible.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.Ops.traverse1_." -> "nonEmptyTraverse_",
    "_root_.cats.Reducible.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.Ops.intercalate1." -> "nonEmptyIntercalate",
    "_root_.cats.Reducible.sequence1_." -> "nonEmptySequence_",
    "_root_.cats.Reducible.Ops.sequence1_." -> "nonEmptySequence_"
  )

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Term.Name => rename(ctx, t, renames)
    }.asPatch
  }

}

// ref: https://github.com/typelevel/cats/pull/1614
case class SimplifyEitherTLift(semanticCtx: SemanticCtx) extends SemanticRewrite(semanticCtx) {

  private[this] val leftSymbol = "_root_.cats.data.EitherTFunctions.left."
  private[this] val rightSymbol = "_root_.cats.data.EitherTFunctions.right."

  private[this] def removeWithLeadingComma(ctx: RewriteCtx, t: Tree): Patch =
    (for {
      leadingComma <- ctx.tokenList.leading(t.tokens.head).find(_.syntax == ",")
    } yield {
      val leadingSpaces = ctx.tokenList.slice(leadingComma, t.tokens.head)
      ctx.removeToken(leadingComma) +
        leadingSpaces.map(ctx.removeToken).asPatch +
        ctx.removeTokens(t.tokens)
    }).asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case Term.ApplyType(Term.Select(_, name), Seq(f, a, b))
          if name.isSymbol(leftSymbol) =>
        ctx.replaceTree(name, "leftT") + removeWithLeadingComma(ctx, a)
      case Term.ApplyType(Term.Select(_, name), Seq(f, a, b))
          if name.isSymbol(rightSymbol) =>
        ctx.replaceTree(name, "pure") + removeWithLeadingComma(ctx, b)
    }.asPatch
  }

}
