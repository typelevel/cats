package cats
package syntax

import cats.functor.Contravariant

trait ContravariantSyntax1 {
  implicit def contravariantSyntaxU[FA](fa: FA)(implicit U: Unapply[Contravariant, FA]): ContravariantOps[U.M, U.A] =
    new ContravariantOps(U.subst(fa))(U.TC)
}

trait ContravariantSyntax extends ContravariantSyntax1 {
  implicit def contravariantSyntax[F[_]: Contravariant, A](fa: F[A]): ContravariantOps[F, A] =
    new ContravariantOps(fa)
}

class ContravariantOps[F[_], A](fa: F[A])(implicit F: Contravariant[F]) {
  def contramap[B](f: B => A): F[B] = F.contramap(fa)(f)
}
