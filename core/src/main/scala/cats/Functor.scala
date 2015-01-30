package cats

import simulacrum._

/*
 * Functor.
 *
 * Must obey the following laws:
 *  - map(fa)(identity) = fa
 *  - map(map(fa)(f1))(f2) = map(fa)(f2 compose f1)
 */
@typeclass trait Functor[F[_]] extends functor.Invariant[F] { self => 
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /** alias for map */
  def fmap[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)

  /** alias for map */
  def imap[A, B](fa: F[A])(f: A <=> B): F[B] = map(fa)(f)

  /////////////////////////////////////////////////////////////////////////
  // derrived functions

  /**
    * Lift a function f to operate on Functors
    */
  def lift[A,B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
    * empty the fa of the values, preserving the structure
    */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /**
    * Tuple the values in fa with the result of applying a function
    * with the value */
  def fproduct[A,B](fa: F[A])(f: A => B): F[(A,B)] = map(fa)(a => a -> f(a))

  /** 
    * Compose this functor F with a functor G to produce a composite
    * Functor on G[F[_]], with a map method which uses an A => B to
    * map a G[F[A]] to a G[F[B]].
    */
  def compose[G[_]: Functor]: Functor[({type λ[α] = F[G[α]]})#λ] =
    new Functor[({type λ[α] = F[G[α]]})#λ] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = 
        self.map(fa)(Functor[G].lift(f))
    }
}
