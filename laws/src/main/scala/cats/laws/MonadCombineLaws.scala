package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadCombine`.
 */
trait MonadCombineLaws[F[_]] extends MonadLaws[F] with FunctorFilterLaws[F] with AlternativeLaws[F] {
  implicit override def F: MonadCombine[F]

  def monadCombineLeftEmpty[A, B](f: A => F[B]): IsEq[F[B]] =
    F.empty[A].flatMap(f) <-> F.empty[B]

  def monadCombineRightEmpty[A, B](fa: F[A]): IsEq[F[B]] =
    fa.flatMap(_ => F.empty[B]) <-> F.empty[B]

  def monadCombineConsistency[A, B](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    fa.filter(f) <-> fa.flatMap(a => if (f(a)) F.pure(a) else F.empty)

  def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEq[F[B]] =
    F.combineK(fa, fa2).flatMap(f) <-> F.combineK(fa flatMap f, fa2 flatMap f)
}

object MonadCombineLaws {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombineLaws[F] =
    new MonadCombineLaws[F] { def F: MonadCombine[F] = ev }
}
