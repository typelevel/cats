package cats

import simulacrum.typeclass

/**
 * Divisible functors
 *
 * Must obey the laws defined in cats.laws.DivisibleLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
 */
@typeclass trait Divisible[F[_]] extends ContravariantSemigroupal[F] { self =>

  /**
   * `unit` produces an instance of `F` for any type `A`
   */
  def unit[A]: F[A]

  /**
   * `contramap2`
   *
   * Given two values in the Divisible context, and a function producing a value of both types,
   * yields an element of the domain of the function lifted into the context.
   */
  def contramap2[A, B, C](fb: F[B], fc: F[C])(f: A => (B, C)): F[A]


  // Technically, this is not correct, as the Applicative is composed with the Divisible, not the other way around
  def composeApplicative[G[_]: Applicative]: Divisible[λ[α => G[F[α]]]] =
    new ComposedApplicativeDivisible[G, F] {
      val F = Applicative[G]
      val G = self
    }

  /**
   * Allows two instances to packaged into an instance over the product
   */
  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    contramap2(fa, fb)(identity)

  /**
   * Lifts a function into the Divisible contravariantly
   */
  def liftD[A, B](f: A => B): F[B] => F[A] =
    contramap2(unit, _: F[B])(((b: B) => (b, b)) compose f)


  override def contramap[A, B](fa: F[A])(f: (B) => A): F[B] =
    liftD(f)(fa)
}

