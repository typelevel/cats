import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleBifunctorInstances extends Template {
  override def range = 1 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleBifunctorInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv._

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleBifunctorInstances {
${if (arity > 1)
      block"""
    -  implicit final def catsStdBifunctorForTuple$arity${`[A0, A(N - 2)]`}: Bifunctor[${`(A..N - 2, *, *)`}] =
    -    new Bifunctor[${`(A..N - 2, *, *)`}] {
    -      def bimap[A, B, C, D](fa: (${`A0, A(N - 2)`}A, B))(f: A => C, g: B => D): (${`A0, A(N - 2)`}C, D) = (${`fa._1..fa._(n - 2)`}f(fa._${arity - 1}), g(fa._$arity))
    -    }"""
    else
      block"""
    -"""}
    |}"""
  }
}
