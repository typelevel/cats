package cats
package syntax

import cats.functor.Invariant

trait InvariantSyntax {
  implicit def invariantSyntax[F[_]: Invariant, A](fa: F[A]): InvariantOps[F, A] =
    new InvariantOps(fa)
}

class InvariantOps[F[_], A](fa: F[A])(implicit F: Invariant[F]) {
  def imap[B](f: A => B)(fi: B => A): F[B] = F.imap(fa)(f)(fi)
}
