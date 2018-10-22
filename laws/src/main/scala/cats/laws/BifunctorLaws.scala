package cats.laws

import cats.Bifunctor
import cats.syntax.bifunctor._

/**
 * Laws that must be obeyed by any `Bifunctor`.
 */
trait BifunctorLaws[F[_, _]] {
  implicit def F: Bifunctor[F]

  def bifunctorIdentity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.bimap(identity, identity) <-> fa

  def bifunctorComposition[A, B, C, X, Y, Z](fa: F[A, X], f: A => B, f2: B => C, g: X => Y, g2: Y => Z): IsEq[F[C, Z]] =
    fa.bimap(f, g).bimap(f2, g2) <-> fa.bimap(f.andThen(f2), g.andThen(g2))

  def bifunctorLeftMapIdentity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.leftMap(identity) <-> fa

  def bifunctorLeftMapComposition[A, B, C, D](fa: F[A, B], f: A => C, g: C => D): IsEq[F[D, B]] =
    fa.leftMap(f).leftMap(g) <-> fa.leftMap(f.andThen(g))

}

object BifunctorLaws {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): BifunctorLaws[F] =
    new BifunctorLaws[F] {
      def F: Bifunctor[F] = ev
    }
}
