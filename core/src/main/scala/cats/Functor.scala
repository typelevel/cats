package cats

import simulacrum._

/**
 * Functor.
 *
 * The name is short for "covariant functor".
 *
 * Must obey the following laws:
 *  - map(fa)(identity) = fa
 *  - map(map(fa)(f1))(f2) = map(fa)(f2 compose f1)
 */
@typeclass trait Functor[F[_]] extends functor.Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /**
   * Alias for map 
   */
  def fmap[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)

  def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = map(fa)(f)

  // derived functions

  /**
   * Lift a function f to operate on Functors
   */
  def lift[A,B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
   * Empty the fa of the values, preserving the structure
   */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /**
   * Tuple the values in fa with the result of applying a function
   * with the value 
   */
  def fproduct[A,B](fa: F[A])(f: A => B): F[(A,B)] = map(fa)(a => a -> f(a))

  /** 
   * Replaces the `A` value in `F[A]` with the supplied value. 
   */
  def as[A, B](fa: F[A], b: B): F[B] =
    map(fa)(_ => b)

  /**
   * Compose this functor F with a functor G to produce a composite
   * Functor on G[F[_]], with a map method which uses an A => B to
   * map a G[F[A]] to a G[F[B]].
   */
  def compose[G[_]](implicit GG: Functor[G]): Functor[λ[α => F[G[α]]]] = new CompositeFunctor[F,G] {
    implicit def F: Functor[F] = self
    implicit def G: Functor[G] = GG
  }
}

trait CompositeFunctor[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
    F.map(fa)(G.lift(f))
}
