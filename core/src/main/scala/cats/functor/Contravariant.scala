package cats
package functor

import simulacrum._

@typeclass trait Contravariant[F[_]] extends Invariant[F] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A <=> B): F[B] = contramap(fa)(f.inverse)
}
