package cats
package laws

import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Functor`.
 */
trait FunctorLaws[F[_]] extends InvariantLaws[F] {
  implicit override def F: Functor[F]

  def covariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.map(identity) <-> fa

  def covariantComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEq[F[C]] =
    fa.map(f).map(g) <-> fa.map(f.andThen(g))
}

object FunctorLaws {
  def apply[F[_]](implicit ev: Functor[F]): FunctorLaws[F] =
    new FunctorLaws[F] { def F: Functor[F] = ev }
}
