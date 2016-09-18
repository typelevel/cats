package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadCombine`.
 */
trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with AlternativeLaws[F] {
  implicit override def F0: MonadCombine[F]
  implicit override def F: Monad[F] = F0.monadInstance

  def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEq[F[B]] =
    F0.combineK(fa, fa2).flatMap(f) <-> F0.combineK(fa flatMap f, fa2 flatMap f)
}

object MonadCombineLaws {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombineLaws[F] =
    new MonadCombineLaws[F] { def F0: MonadCombine[F] = ev }
}
