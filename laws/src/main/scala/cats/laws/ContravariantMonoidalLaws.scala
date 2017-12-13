package cats
package laws

import cats.ContravariantMonoidal
import cats.syntax.contravariant._
import cats.syntax.contravariantSemigroupal._

/**
 * Laws that must hold for any `cats.ContravariantMonoidal`.
 */
trait ContravariantMonoidalLaws[F[_]] extends ContravariantSemigroupalLaws[F] {
  implicit override def F: ContravariantMonoidal[F]

  def contravariantMonoidalUnitRight[A](fa: F[A]): IsEq[F[A]] =
    (fa, F.conquer[A]).contramapN(delta[A]) <-> fa

  def contravariantMonoidalUnitLeft[A](fa: F[A]): IsEq[F[A]] =
    (F.conquer[A], fa).contramapN(delta[A]) <-> fa

  def contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C](fa: F[A], f: B => (A, C)): IsEq[F[B]] =
    (fa, F.conquer[C]).contramapN(f) <-> fa.contramap(f andThen (_._1))

  def contravariantMonoidalContramap2CompatibleContramapRight[A, B, C](fa: F[A], f: C => (B, A)): IsEq[F[C]] =
    (F.conquer[B], fa).contramapN(f) <-> fa.contramap(f andThen (_._2))
}

object ContravariantMonoidalLaws {
  def apply[F[_]](implicit ev: ContravariantMonoidal[F]): ContravariantMonoidalLaws[F] =
    new ContravariantMonoidalLaws[F] { def F: ContravariantMonoidal[F] = ev }
}
