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

abstract class SemigroupalOps[F[_], A] extends Semigroupal.Ops[F, A]
