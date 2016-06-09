package cats

import cats.functor.Contravariant

import simulacrum.typeclass

/**
 * Functor.
 *
 * The name is short for "covariant functor".
 *
 * Must obey the laws defined in cats.laws.FunctorLaws.
 */
@typeclass trait Functor[F[_]] extends functor.Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = map(fa)(f)

  // derived methods

  /**
   * Lifts natural subtyping covariance of covariant Functors.
   * could be implemented as map(identity), but the Functor laws say this is equivalent
   */
  def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]

  /**
   * Lift a function f to operate on Functors
   */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
   * Empty the fa of the values, preserving the structure
   */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /**
   * Tuple the values in fa with the result of applying a function
   * with the value
   */
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  /**
   * Replaces the `A` value in `F[A]` with the supplied value.
   */
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] =
    new ComposedFunctor[F, G] {
      val F = self
      val G = Functor[G]
    }

  override def composeContravariant[G[_]: Contravariant]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedCovariantContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }
}
