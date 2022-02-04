import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenFunctorTupleNSyntax extends Template {
  // we generate syntax for Tuple3..22 because already there is [[cats.syntax.FunctorTuple2Ops]].
  override def range = 3 to maxArity
  override def filename(root: sbt.File): File =
    root / "cats" / "syntax" / "FunctorTupleNSyntax.scala"

  override def content(tv: TemplateVals): String = {
    import tv._

    val generatedFunctions: String =
      (1 to arity).map { n =>
        s"""
          -  /**
          -   * Lifts [[Tuple$arity._$n]] into `F[_]`.
          -   */
          -  def _${n}F(implicit F: Functor[F]): F[A${n-1}] = F.map(ftuple)(_._$n)
          -
        """
      }.mkString("\n")

    block"""
    |
    |package cats
    |package syntax
    |
    |trait FunctorTupleNSyntax {
      -  implicit final def catsSyntaxFunctorTuple${arity}Ops[F[_], ${`A..N`}](ftuple: F[(${`A..N`})]): FunctorTuple${arity}Ops[F, ${`A..N`}] = new FunctorTuple${arity}Ops[F, ${`A..N`}](ftuple)
      -
      -  private[syntax] final class FunctorTuple${arity}Ops[F[_], ${`A..N`}](ftuple: F[(${`A..N`})]) extends Serializable {
           $generatedFunctions
      -  }
      -
    |}"""
  }
}
