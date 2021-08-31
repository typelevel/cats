import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleBitraverseInstances extends Template {
  override def range = 1 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleBitraverseInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv._

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleBitraverseInstances {
${if (arity > 1)
      block"""
    -  implicit final def catsStdBitraverseForTuple$arity${`[A0, A(N - 2)]`}: Bitraverse[${`(A..N - 2, *, *)`}] =
    -    new Bitraverse[${`(A..N - 2, *, *)`}] {
    -      def bitraverse[G[_], A, B, C, D](fa: (${`A0, A(N - 2)`}A, B))(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[(${`A0, A(N - 2)`}C, D)] =
    -        G.tuple$arity(${`pure(fa._1..(n - 2))`}f(fa._${arity - 1}), g(fa._$arity))
    -      def bifoldLeft[A, B, C](fa: (${`A0, A(N - 2)`}A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
    -        g(f(c, fa._${arity - 1}), fa._$arity)
    -      def bifoldRight[A, B, C](fa: (${`A0, A(N - 2)`}A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    -        g(fa._$arity, f(fa._${arity - 1}, c))
    -    }"""
    else
      block"""
    -"""}
    |}"""
  }
}
