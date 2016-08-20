package cats
package laws

import cats.arrow.Choice
import cats.data.Xor
import cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Choice`.
 */
trait ChoiceLaws[F[_, _]] extends CategoryLaws[F] {
  implicit override def F: Choice[F]

  def choiceCompositionDistributivity[A, B, C, D](fac: F[A, C], fbc: F[B, C], fcd: F[C, D]): IsEq[F[Xor[A, B], D]] =
    (F.choice(fac, fbc) andThen fcd) <-> F.choice(fac andThen fcd, fbc andThen fcd)
}

object ChoiceLaws {
  def apply[F[_, _]](implicit ev: Choice[F]): ChoiceLaws[F] =
    new ChoiceLaws[F] { def F: Choice[F] = ev }
}
