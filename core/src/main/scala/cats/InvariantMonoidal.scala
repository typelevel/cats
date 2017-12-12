package cats

import simulacrum.typeclass

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
@typeclass trait InvariantMonoidal[F[_]] extends InvariantSemigroupal[F] {
  /**
    * `pure` lifts any value into a Monoidal Functor.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    *
    * scala> InvariantMonoidal[Option].pure(10)
    * res0: Option[Int] = Some(10)
    * }}}
    */
  def pure[A](a: A): F[A]
}

object InvariantMonoidal {
  def monoid[F[_], A](implicit f: InvariantMonoidal[F], monoid: Monoid[A]): Monoid[F[A]] =
    new InvariantMonoidalMonoid[F, A](f, monoid)
}

private[cats] class InvariantMonoidalMonoid[F[_], A](f: InvariantMonoidal[F], monoid: Monoid[A]) extends InvariantSemigroupalSemigroup(f, monoid) with Monoid[F[A]] {
  def empty: F[A] = f.pure(monoid.empty)
}
