package cats
package syntax

import cats.functor.Invariant

trait InvariantSyntax1 {
  implicit def invariantSyntaxU[FA](fa: FA)(implicit U: Unapply[Invariant, FA]): InvariantOps[U.M, U.A] =
    new InvariantOps(U.subst(fa))(U.TC)
}

trait InvariantSyntax extends InvariantSyntax1 {
  implicit def invariantSyntax[F[_] : Invariant, A](fa: F[A]): InvariantOps[F, A] =
    new InvariantOps(fa)
}

class InvariantOps[F[_], A](fa: F[A])(implicit F: Invariant[F]) {
  def imap[B](f: A => B)(fi: B => A): F[B] = F.imap(fa)(f)(fi)
}
