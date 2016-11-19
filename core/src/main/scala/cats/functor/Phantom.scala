package cats
package functor

import simulacrum.typeclass

/**
  * Phantom-variant Functor.
  * Values of type A must not appear in F[A].
  * Natural result of a functor being both covariant and contravariant.
  */
@typeclass trait Phantom[F[_]] extends Contravariant[F] {
  def functor: Functor[F] = new Functor[F] {
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = pmap(fa)
  }

  def pmap[A, B](fa: F[A]): F[B]

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = pmap[A, B](fa)

  override def contramap[A, B](fa: F[A])(f: B => A): F[B] = pmap[A, B](fa)
}
