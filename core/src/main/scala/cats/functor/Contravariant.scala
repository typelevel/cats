package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
@typeclass trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)
}
