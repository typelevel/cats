package cats
package syntax

trait SemigroupalSyntax {
  implicit final def catsSyntaxSemigroupal[F[_], A](fa: F[A])(implicit F: Semigroupal[F]): SemigroupalOps[F, A] =
    new SemigroupalOps[F, A] {
      type TypeClassType = Semigroupal[F]

      val self = fa
      val typeClassInstance = F
    }
}

abstract class SemigroupalOps[F[_], A] extends Semigroupal.Ops[F, A] {

  @deprecated("Replaced by an apply syntax, e.g. instead of (a |@| b).map(...) use (a, b).mapN(...)", "1.0.0-MF")
  final private[syntax] def |@|[B](fb: F[B]): SemigroupalBuilder[F]#SemigroupalBuilder2[A, B] =
    new SemigroupalBuilder[F] |@| self |@| fb

}
