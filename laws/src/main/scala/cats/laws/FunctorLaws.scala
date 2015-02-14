package cats.laws

import cats.Functor
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any [[Functor]].
 */
trait FunctorLaws[F[_]] extends InvariantLaws[F] {
  implicit override def F: Functor[F]

  def covariantIdentity[A](fa: F[A]): (F[A], F[A]) =
    fa.map(identity) -> fa

  def covariantComposition[A, B, C](fa: F[A], f: A => B, g: B => C): (F[C], F[C]) =
    fa.map(f).map(g) -> fa.map(g compose f)
}

object FunctorLaws {
  def apply[F[_]](implicit ev: Functor[F]): FunctorLaws[F] =
    new FunctorLaws[F] { def F = ev }
}
