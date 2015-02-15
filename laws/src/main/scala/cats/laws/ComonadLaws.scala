package cats.laws

import cats.Comonad
import cats.syntax.coflatMap._
import cats.syntax.comonad._

/**
 * Laws that must be obeyed by any [[Comonad]].
 */
trait ComonadLaws[F[_]] extends CoflatMapLaws[F] {
  implicit override def F: Comonad[F]

  def comonadLeftIdentity[A](fa: F[A]): (F[A], F[A]) =
    fa.coflatMap(_.extract) -> fa

  def comonadRightIdentity[A, B](fa: F[A], f: F[A] => B): (B, B) =
    fa.coflatMap(f).extract -> f(fa)
}

object ComonadLaws {
  def apply[F[_]](implicit ev: Comonad[F]): ComonadLaws[F] =
    new ComonadLaws[F] { def F = ev }
}
