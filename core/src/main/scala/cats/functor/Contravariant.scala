package cats
package functor

import simulacrum._

@typeclass trait Contravariant[F[_]] extends Invariant[F] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B, fi: B => A): F[B] = contramap(fa)(fi)
}
