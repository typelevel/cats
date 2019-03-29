package cats

import simulacrum.typeclass

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
@typeclass trait InvariantMonoidal[F[_]] extends InvariantSemigroupal[F] {

  /**
   * `point` lifts any value into a Monoidal Functor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> InvariantMonoidal[Option].point(10)
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def point[A](a: A): F[A] = imap(unit)(_ => a)(_ => ())

  def unit: F[Unit]

}

object InvariantMonoidal {

  /**
   * Gives a `Monoid` instance if A itself has a `Monoid` instance.
   */
  def monoid[F[_], A](implicit F: InvariantMonoidal[F], A: Monoid[A]): Monoid[F[A]] =
    new InvariantMonoidalMonoid[F, A](F, A)
}

private[cats] class InvariantMonoidalMonoid[F[_], A](f: InvariantMonoidal[F], monoid: Monoid[A])
    extends InvariantSemigroupalSemigroup(f, monoid)
    with Monoid[F[A]] {
  def empty: F[A] = f.point(monoid.empty)
}
