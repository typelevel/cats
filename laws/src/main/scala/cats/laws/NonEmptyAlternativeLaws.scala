package cats
package laws

import cats.syntax.all._

trait NonEmptyAlternativeLaws[F[_]] extends ApplicativeLaws[F] with SemigroupKLaws[F] {
  implicit override def F: NonEmptyAlternative[F]
  implicit def algebra[A]: Semigroup[F[A]] = F.algebra[A]

  def nonEmptyAlternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2).map(f)) <-> ((fa.map(f)) |+| (fa2.map(f)))

  def nonEmptyAlternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff |+| fg).ap(fa)) <-> ((ff.ap(fa)) |+| (fg.ap(fa)))

  def nonEmptyAlternativePrependKConsitentWithPureAndCombineK[A](fa: F[A], a: A): IsEq[F[A]] =
    fa.prependK(a) <-> (a.pure[F] <+> fa)

  def nonEmptyAlternativeAppendKConsitentWithPureAndCombineK[A](fa: F[A], a: A): IsEq[F[A]] =
    fa.appendK(a) <-> (fa <+> a.pure[F])
}

object NonEmptyAlternativeLaws {
  def apply[F[_]](implicit ev: NonEmptyAlternative[F]): NonEmptyAlternativeLaws[F] =
    new NonEmptyAlternativeLaws[F] { def F: NonEmptyAlternative[F] = ev }

  def composed[M[_], N[_]](implicit
    M: NonEmptyAlternative[M],
    N: Applicative[N]
  ): NonEmptyAlternativeLaws[λ[α => M[N[α]]]] =
    new NonEmptyAlternativeLaws[λ[α => M[N[α]]]] {
      def F: NonEmptyAlternative[λ[α => M[N[α]]]] = M.compose[N]
    }
}
