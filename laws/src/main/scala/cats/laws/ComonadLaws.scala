package cats
package laws

import cats.data.Cokleisli
import cats.syntax.coflatMap._
import cats.syntax.comonad._

/**
 * Laws that must be obeyed by any [[Comonad]].
 */
trait ComonadLaws[F[_]] extends CoflatMapLaws[F] {
  implicit override def F: Comonad[F]

  def comonadLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.coflatMap(_.extract) <-> fa

  def comonadRightIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    fa.coflatMap(f).extract <-> f(fa)

  /**
   * `extract` is the left identity element under left-to-right composition of
   * [[cats.data.Cokleisli]] arrows. This is analogous to [[comonadLeftIdentity]].
   */
  def cokleisliLeftIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    (Cokleisli(F.extract[A]) andThen Cokleisli(f)).run(fa) <-> f(fa)

  /**
   * `extract` is the right identity element under left-to-right composition of
   * [[cats.data.Cokleisli]] arrows. This is analogous to [[comonadRightIdentity]].
   */
  def cokleisliRightIdentity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    (Cokleisli(f) andThen Cokleisli(F.extract[B])).run(fa) <-> f(fa)
}

object ComonadLaws {
  def apply[F[_]](implicit ev: Comonad[F]): ComonadLaws[F] =
    new ComonadLaws[F] { def F: Comonad[F] = ev }
}
