package cats

import simulacrum.typeclass

/**
 * [[Decideable]] functors are functors that supply
 * a `decide` operation allowing choices to be made.
 *
 * This is comparable to [[Alternative]] in the
 * covariant case.
 *
 * Must obey laws in cats.laws.DecideableLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html#g:2
 */
@typeclass trait Decideable[F[_]] extends ContravariantMonoidal[F] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contramap(sum(fa, fb))(f)

  def chosen[B, C](fb: F[B], fc: F[C]): F[Either[B, C]] =
    decide(fb, fc)(identity[Either[B, C]])

  def zero: F[Nothing] = trivial[Nothing]
}
