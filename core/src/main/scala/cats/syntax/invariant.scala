package cats
package syntax

import cats.functor.Invariant

trait InvariantSyntax1 {
  implicit def invariantSyntaxU[FA](fa: FA)(implicit U: Unapply[Invariant, FA]): Invariant.Ops[U.M, U.A] =
    new Invariant.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait InvariantSyntax extends Invariant.ToInvariantOps with InvariantSyntax1
