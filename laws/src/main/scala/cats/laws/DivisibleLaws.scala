package cats
package laws

import cats.Divisible
import cats.syntax.contravariant._
import cats.syntax.divisible._

/**
 * Laws that must hold for any `cats.Divisible`.
 */
trait DivisibleLaws[F[_]] extends ContravariantLaws[F] {
  implicit override def F: Divisible[F]

  /**
   * The traditional diagonal map
   */
  def delta[A](a: A): (A, A) = (a, a)

  def divisibleUnitRight[A](fa: F[A]): IsEq[F[A]] =
    fa.contramap2(F.unit)(delta[A]) <-> fa

  def divisibleUnitLeft[A](fa: F[A]): IsEq[F[A]] =
    (F.unit).contramap2(fa)(delta[A]) <-> fa

  def divisibleContramap2CompatibleContramapLeft[A, B, C](fa: F[A], f: B => (A, C)): IsEq[F[B]] =
    fa.contramap2(F.unit)(f) <-> fa.contramap(f andThen (_._1))

  def divisibleContramap2CompatibleContramapRight[A, B, C](fa: F[A], f: C => (B, A)): IsEq[F[C]] =
    (F.unit).contramap2(fa)(f) <-> fa.contramap(f andThen (_._2))

  def divisibleContramap2DiagonalAssociates[A](m: F[A], n: F[A], o: F[A]): IsEq[F[A]] =
    (m.contramap2(n)(delta[A])).contramap2(o)(delta[A]) <-> m.contramap2(n.contramap2(o)(delta[A]))(delta[A])
}

object DivisibleLaws {
  def apply[F[_]](implicit ev: Divisible[F]): DivisibleLaws[F] =
    new DivisibleLaws[F] { def F: Divisible[F] = ev }
}
