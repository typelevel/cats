package cats
package laws

import cats.syntax.flatMap._

/**
 * Laws that must be obeyed by any `MonadFilter`.
 */
trait MonadFilterLaws[F[_]] extends MonadLaws[F] {
  implicit override def F: MonadFilter[F]

  def monadFilterLeftEmpty[A, B](f: A => F[B]): IsEq[F[B]] =
    F.empty[A].flatMap(f) <-> F.empty[B]

  def monadFilterRightEmpty[A, B](fa: F[A]): IsEq[F[B]] =
    fa.flatMap(_ => F.empty[B]) <-> F.empty[B]
}

object MonadFilterLaws {
  def apply[F[_]](implicit ev: MonadFilter[F]): MonadFilterLaws[F] =
    new MonadFilterLaws[F] { def F: MonadFilter[F] = ev }
}
