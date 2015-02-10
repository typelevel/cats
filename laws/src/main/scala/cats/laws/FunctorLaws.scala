package cats.laws

import cats.Functor
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any [[Functor]].
 */
class FunctorLaws[F[_]](implicit F: Functor[F]) extends InvariantLaws[F] {
  def covariantIdentity[A](fa: F[A]): (F[A], F[A]) =
    fa.map(identity) -> fa

  def covariantComposition[A, B, C](fa: F[A], f: A => B, g: B => C): (F[C], F[C]) =
    fa.map(f).map(g) -> fa.map(g compose f)
}
