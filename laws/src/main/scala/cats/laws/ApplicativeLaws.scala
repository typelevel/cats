package cats
package laws

import cats.syntax.apply._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Applicative`.
 */
trait ApplicativeLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.pure((a: A) => a).ap(fa) <-> fa

  def applicativeHomomorphism[A, B](a: A, f: A => B): IsEq[F[B]] =
    F.pure(f).ap(F.pure(a)) <-> F.pure(f(a))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]): IsEq[F[B]] =
    ff.ap(F.pure(a)) <-> F.pure((f: A => B) => f(a)).ap(ff)

  def applicativeMap[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> F.pure(f).ap(fa)

  /**
   * This law is [[applyComposition]] stated in terms of `pure`. It is a
   * combination of [[applyComposition]] and [[applicativeMap]] and hence not
   * strictly necessary.
   */
  def applicativeComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    F.pure(compose).ap(fbc).ap(fab).ap(fa) <-> fbc.ap(fab.ap(fa))
  }

  def apProductConsistent[A, B](fa: F[A], f: F[A => B]): IsEq[F[B]] =
    F.ap(f)(fa) <-> F.map(F.product(f, fa)) { case (f, a) => f(a) }

  def applicativeUnit[A](a: A): IsEq[F[A]] =
    F.unit.map(_ => a) <-> F.pure(a)

  // The following are the lax monoidal functor identity laws - the associativity law is covered by
  // Semigroupal's associativity law.

  def monoidalLeftIdentity[A](fa: F[A]): (F[(Unit, A)], F[A]) =
    (F.product(F.pure(()), fa), fa)

  def monoidalRightIdentity[A](fa: F[A]): (F[(A, Unit)], F[A]) =
    (F.product(fa, F.pure(())), fa)
}

object ApplicativeLaws {
  def apply[F[_]](implicit ev: Applicative[F]): ApplicativeLaws[F] =
    new ApplicativeLaws[F] { def F: Applicative[F] = ev }
}
