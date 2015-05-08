package cats
package laws

import cats.syntax.apply._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any [[Applicative]].
 */
trait ApplicativeLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.ap(F.pure((a: A) => a)) <-> fa

  def applicativeHomomorphism[A, B](a: A, f: A => B): IsEq[F[B]] =
    F.pure(a).ap(F.pure(f)) <-> F.pure(f(a))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]): IsEq[F[B]] =
    F.pure(a).ap(ff) <-> ff.ap(F.pure(f => f(a)))

  def applicativeMap[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> fa.ap(F.pure(f))

  /**
   * This law is [[applyComposition]] stated in terms of `pure`. It is a
   * combination of [[applyComposition]] and [[applicativeMap]] and hence not
   * strictly necessary.
   */
  def applicativeComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    fa.ap(fab.ap(fbc.ap(F.pure(compose)))) <-> fa.ap(fab).ap(fbc)
  }
}

object ApplicativeLaws {
  def apply[F[_]](implicit ev: Applicative[F]): ApplicativeLaws[F] =
    new ApplicativeLaws[F] { def F: Applicative[F] = ev }
}
