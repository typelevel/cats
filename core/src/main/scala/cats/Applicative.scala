package cats

import cats.arrow.Arrow
import cats.instances.list._
import simulacrum.typeclass

/**
 * Applicative functor.
 *
 * Allows application of a function in an Applicative context to a value in an Applicative context
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 * Also: [[http://staff.city.ac.uk/~ross/papers/Applicative.pdf Applicative programming with effects]]
 *
 * Must obey the laws defined in cats.laws.ApplicativeLaws.
 */
@typeclass trait Applicative[F[_]] extends Apply[F] { self =>

  /**
   * `pure` lifts any value into the Applicative Functor.
   *
   * Applicative[Option].pure(10) = Some(10)
   */
  def pure[A](x: A): F[A]

  /**
   * Returns an `F[Unit]` value, equivalent with `pure(())`.
   *
   * A useful shorthand, also allowing implementations to optimize the
   * returned reference (e.g. it can be a `val`).
   */
  def unit: F[Unit] = pure(())

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  /**
   * Given `fa` and `n`, apply `fa` `n` times to construct an `F[List[A]]` value.
   */
  def replicateA[A](n: Int, fa: F[A]): F[List[A]] =
    Traverse[List].sequence(List.fill(n)(fa))(this)

  def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]] =
    new ComposedApplicative[F, G] {
      val F = self
      val G = Applicative[G]
    }

  def composeContravariantMonoidal[G[_]: ContravariantMonoidal]: ContravariantMonoidal[λ[α => F[G[α]]]] =
    new ComposedApplicativeContravariantMonoidal[F, G] {
      val F = self
      val G = ContravariantMonoidal[G]
    }

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into F.
   */
  def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit] =
    if (cond) pure(()) else void(f)

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into F.
   */
  def whenA[A](cond: Boolean)(f: => F[A]): F[Unit] =
    if (cond) void(f) else pure(())
}

object Applicative {
  def monoid[F[_], A](implicit f: Applicative[F], monoid: Monoid[A]): Monoid[F[A]] =
    new ApplicativeMonoid[F, A](f, monoid)

  /**
   * Creates a semigroupal functor for `F`, holding domain fixed and combining
   * over the codomain.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.Applicative.catsApplicativeForArrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val double: Int => Int = 2*_
   * scala> val f: Int => (Long, Int) = catsApplicativeForArrow.product(toLong, double)
   * scala> f(3)
   * res0: (Long, Int) = (3,6)
   * }}}
   */
  implicit def catsApplicativeForArrow[F[_, _], A](implicit F: Arrow[F]): Applicative[F[A, ?]] =
    new ArrowApplicative[F, A](F)
}

private[cats] class ApplicativeMonoid[F[_], A](f: Applicative[F], monoid: Monoid[A]) extends ApplySemigroup(f, monoid) with Monoid[F[A]] {
  def empty: F[A] = f.pure(monoid.empty)
}

private[cats] class ArrowApplicative[F[_, _], A](F: Arrow[F]) extends Applicative[F[A, ?]] {
  def pure[B](b: B): F[A, B] = F.lift[A, B](_ => b)
  override def map[B, C](fb: F[A, B])(f: B => C): F[A, C] = F.rmap(fb)(f)
  def ap[B, C](ff: F[A, B => C])(fb: F[A, B]): F[A, C] =
    F.rmap(F.andThen(F.lift((x: A) => (x, x)), F.split(ff, fb)))(tup => tup._1(tup._2))
  override def product[B, C](fb: F[A, B], fc: F[A, C]): F[A, (B, C)] =
    F.andThen(F.lift((x: A) => (x, x)), F.split(fb, fc))
}
