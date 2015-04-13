package cats
package syntax

import cats.functor.Contravariant

trait ContravariantSyntax {
  implicit def invariantSyntax[FA](fa: FA)(implicit U: Unapply[Contravariant, FA]): ContravariantOps[U.M, U.A] =
    new ContravariantOps(U.subst(fa))(U.TC)
}

class ContravariantOps[F[_], A](fa: F[A])(implicit F: Contravariant[F]) {
  def contramap[B](f: B => A): F[B] = F.contramap(fa)(f)
}
