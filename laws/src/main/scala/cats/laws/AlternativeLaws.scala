package cats
package laws

import cats.syntax.all._

trait AlternativeLaws[F[_]] extends NonEmptyAlternativeLaws[F] with MonoidKLaws[F] {
  implicit override def F: Alternative[F]
  implicit override def algebra[A]: Monoid[F[A]] = F.algebra[A]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (ff.ap(F.empty[A])) <-> F.empty[B]

  // Perhaps should be deprecated in favor of nonEmptyAlternativeLeftDistributivity
  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    nonEmptyAlternativeLeftDistributivity[A, B](fa, fa2, f)

  // Perhaps should be deprecated in favor of nonEmptyAlternativeRightDistributivity
  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    nonEmptyAlternativeRightDistributivity(fa, ff, fg)
}

object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F: Alternative[F] = ev }

  def composed[M[_], N[_]](implicit M: Alternative[M], N: Applicative[N]): AlternativeLaws[λ[α => M[N[α]]]] =
    new AlternativeLaws[λ[α => M[N[α]]]] {
      def F: Alternative[λ[α => M[N[α]]]] = M.compose[N]
    }
}
