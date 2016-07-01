package cats
package syntax

import cats.functor.Contravariant

private[syntax] trait ContravariantSyntax1 {
  implicit def catsSyntaxUContravariant[FA](fa: FA)(implicit U: Unapply[Contravariant, FA]): Contravariant.Ops[U.M, U.A] =
    new Contravariant.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait ContravariantSyntax extends Contravariant.ToContravariantOps with ContravariantSyntax1

