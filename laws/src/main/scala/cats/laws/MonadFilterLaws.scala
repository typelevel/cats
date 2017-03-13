package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadFilter`.
 */
trait MonadFilterLaws[F[_]] extends MonadLaws[F] with FunctorFilterLaws[F] {
  implicit override def F: MonadFilter[F]

  def monadFilterLeftEmpty[A, B](f: A => F[B]): IsEq[F[B]] =
    F.empty[A].flatMap(f) <-> F.empty[B]

  def monadFilterRightEmpty[A, B](fa: F[A]): IsEq[F[B]] =
    fa.flatMap(_ => F.empty[B]) <-> F.empty[B]

  def monadFilterConsistency[A, B](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    fa.filter(f) <-> fa.flatMap(a => if (f(a)) F.pure(a) else F.empty)
}

object MonadFilterLaws {
  def apply[F[_]](implicit ev: MonadFilter[F]): MonadFilterLaws[F] =
    new MonadFilterLaws[F] { def F: MonadFilter[F] = ev }
}
