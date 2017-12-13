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
@typeclass trait ContravariantMonoidal[F[_]] extends ContravariantSemigroupal[F] with InvariantMonoidal[F] {
  /**
   * `conquer` produces an instance of `F` for any type `A`
   * that is trivial with respect to `contramap2` along
   * the diagonal
   */
  def conquer[A]: F[A] = contramap(unit)(_ => ())

  override def monoid[A](implicit A: Monoid[A]): Monoid[F[A]] =
    ContravariantMonoidal.monoid(this)
}
object ContravariantMonoidal extends SemigroupalArityFunctions {
  def monoid[F[_], A](implicit f: ContravariantMonoidal[F]): Monoid[F[A]] =
    new ContravariantMonoidalMonoid[F, A](f)
}

private[cats] class ContravariantMonoidalMonoid[F[_], A](f: ContravariantMonoidal[F]) extends ContravariantSemigroupalSemigroup[F, A](f) with Monoid[F[A]] {
  def empty: F[A] = f.conquer
}
