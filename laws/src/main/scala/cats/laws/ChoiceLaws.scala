package cats
package laws

import cats.arrow.Choice
import cats.syntax.choice._
import cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Choice`.
 */
trait ChoiceLaws[F[_, _]] extends CategoryLaws[F] {
  implicit override def F: Choice[F]

  def choiceCompositionDistributivity[A, B, C, D](fac: F[A, C], fbc: F[B, C], fcd: F[C, D]): IsEq[F[Either[A, B], D]] =
    ((fac ||| fbc) >>> fcd) <-> ((fac >>> fcd) ||| (fbc >>> fcd))
}

object ChoiceLaws {
  def apply[F[_, _]](implicit ev: Choice[F]): ChoiceLaws[F] =
    new ChoiceLaws[F] { def F: Choice[F] = ev }
}
