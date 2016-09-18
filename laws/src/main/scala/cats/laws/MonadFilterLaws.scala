package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadFilter`.
 */
trait MonadFilterLaws[F[_]] extends MonadLaws[F] with FunctorFilterLaws[F] {
  implicit override def F0: MonadFilter[F]
  implicit override def F: Monad[F] = F0.monadInstance

  def monadFilterLeftEmpty[A, B](f: A => F[B]): IsEq[F[B]] =
    F0.empty[A].flatMap(f) <-> F0.empty[B]

  def monadFilterRightEmpty[A, B](fa: F[A]): IsEq[F[B]] =
    fa.flatMap(_ => F0.empty[B]) <-> F0.empty[B]

  def monadFilterConsistency[A, B](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    fa.filter(f) <-> fa.flatMap(a => if (f(a)) F.pure(a) else F0.empty)
}

object MonadFilterLaws {
  def apply[F[_]](implicit ev: MonadFilter[F]): MonadFilterLaws[F] =
    new MonadFilterLaws[F] { def F0: MonadFilter[F] = ev }
}
