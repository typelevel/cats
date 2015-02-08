package cats.laws

import cats.Applicative
import cats.syntax.apply._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any [[Applicative]].
 */
class ApplicativeLaws[F[_]](implicit F: Applicative[F]) extends ApplyLaws[F] {
  def applicativeIdentity[A](fa: F[A]): (F[A], F[A]) =
    fa.apply(F.pure((a: A) => a)) -> fa

  def applicativeHomomorphism[A, B](a: A, f: A => B): (F[B], F[B]) =
    F.pure(a).apply(F.pure(f)) -> F.pure(f(a))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]): (F[B], F[B]) =
    F.pure(a).apply(ff) -> ff.apply(F.pure(f => f(a)))

  def applicativeMap[A, B](fa: F[A], f: A => B): (F[B], F[B]) =
    fa.map(f) -> fa.apply(F.pure(f))

  /**
   * This law is [[applyComposition]] stated in terms of [[Applicative.pure]].
   * It is a combination of [[applyComposition]] and [[applicativeMap]] and
   * hence not strictly necessary.
   */
  def applicativeComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): (F[C], F[C]) = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    fa.apply(fab.apply(fbc.apply(F.pure(compose)))) -> fa.apply(fab).apply(fbc)
  }
}
