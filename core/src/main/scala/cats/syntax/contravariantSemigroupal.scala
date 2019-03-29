package cats
package syntax

import cats.ContravariantSemigroupal

trait ContravariantSemigroupalSyntax extends TupleSemigroupalSyntax {
  implicit final def catsSyntaxContravariantSemigroupal[F[_], A](
    fa: F[A]
  )(implicit F: ContravariantSemigroupal[F]): ContravariantSemigroupal.Ops[F, A] =
    new ContravariantSemigroupal.Ops[F, A] {
      type TypeClassType = ContravariantSemigroupal[F]

      val self = fa
      val typeClassInstance = F
    }
}
