package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any [[MonadCombine]].
 */
trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with AlternativeLaws[F] {
  implicit override def F: MonadCombine[F]

  def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEq[F[B]] =
    F.combine(fa, fa2).flatMap(f) <-> F.combine(fa flatMap f, fa2 flatMap f)
}

object MonadCombineLaws {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombineLaws[F] =
    new MonadCombineLaws[F] { def F: MonadCombine[F] = ev }
}
