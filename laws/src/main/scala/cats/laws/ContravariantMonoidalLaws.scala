package cats
package laws

import cats.ContravariantMonoidal
import cats.syntax.contravariant._
import cats.syntax.contravariantSemigroupal._

/**
 * Laws that must hold for any `cats.ContravariantMonoidal`.
 */
trait ContravariantMonoidalLaws[F[_]] extends ContravariantLaws[F] {
  implicit override def F: ContravariantMonoidal[F]

  /**
   * The traditional diagonal map
   */
  def delta[A](a: A): (A, A) = (a, a)

  def contravariantMonoidalUnitRight[A](fa: F[A]): IsEq[F[A]] =
    fa.contramap2(F.unit)(delta[A]) <-> fa

  def contravariantMonoidalUnitLeft[A](fa: F[A]): IsEq[F[A]] =
    (F.unit).contramap2(fa)(delta[A]) <-> fa

  def contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C](fa: F[A], f: B => (A, C)): IsEq[F[B]] =
    fa.contramap2(F.unit)(f) <-> fa.contramap(f andThen (_._1))

  def contravariantMonoidalContramap2CompatibleContramapRight[A, B, C](fa: F[A], f: C => (B, A)): IsEq[F[C]] =
    (F.unit).contramap2(fa)(f) <-> fa.contramap(f andThen (_._2))

  def contravariantMonoidalContramap2DiagonalAssociates[A](m: F[A], n: F[A], o: F[A]): IsEq[F[A]] =
    (m.contramap2(n)(delta[A])).contramap2(o)(delta[A]) <-> m.contramap2(n.contramap2(o)(delta[A]))(delta[A])
}

object ContravariantMonoidalLaws {
  def apply[F[_]](implicit ev: ContravariantMonoidal[F]): ContravariantMonoidalLaws[F] =
    new ContravariantMonoidalLaws[F] { def F: ContravariantMonoidal[F] = ev }
}
