package cats

import simulacrum.typeclass

/**
 * [[ContravariantMonoidal]] functors are functors that supply
 * a unit along the diagonal map for the `contramap2` operation.
 *
 * Must obey the laws defined in cats.laws.ContravariantMonoidalLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
 */
@typeclass trait ContravariantMonoidal[F[_]] extends ContravariantSemigroupal[F] { self =>
  /**
   * `unit` produces an instance of `F` for any type `A`
   * that is trivial with respect to `contramap2` along
   * the diagonal
   */
  def unit[A]: F[A]

  def liftContravariant[A, B](f: A => B): F[B] => F[A] =
    ContravariantMonoidal.contramap2(unit[B], _: F[B])(((b: B) => (b, b)) compose f)(self, self)

  // Technically, this is not correct, as the Applicative is composed with the ContravariantMonoidal, not the other way around
  def composeApplicative[G[_]: Applicative]: ContravariantMonoidal[λ[α => G[F[α]]]] =
    new ComposedApplicativeContravariantMonoidal[G, F] {
      val F = Applicative[G]
      val G = self
    }
}
object ContravariantMonoidal extends SemigroupalArityFunctions {
}
