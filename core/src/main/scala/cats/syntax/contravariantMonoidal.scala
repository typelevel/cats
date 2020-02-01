package cats
package syntax

import cats.ContravariantMonoidal

trait ContravariantMonoidalSyntax {
  implicit final def catsSyntaxContravariantMonoidal[F[_], A](
    fa: F[A]
  )(implicit F: ContravariantMonoidal[F]): ContravariantMonoidalOps[F, A] =
    new ContravariantMonoidalOps[F, A] {
      type TypeClassType = ContravariantMonoidal[F]

      val self = fa
      val typeClassInstance = F
    }
}
abstract class ContravariantMonoidalOps[F[_], A] extends ContravariantMonoidal.Ops[F, A]
