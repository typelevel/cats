package cats
package laws

import cats.arrow.Split
import cats.syntax.compose._
import cats.syntax.split._

/**
 * Laws that must be obeyed by any `cats.arrow.Split`.
 */
trait SplitLaws[F[_, _]] extends ComposeLaws[F] {
  implicit override def F: Split[F]

  def splitInterchange[A1, A2, A3, B1, B2, B3](f1: F[A1, A2], f2: F[A2, A3],
                                               g1: F[B1, B2], g2: F[B2, B3]): IsEq[F[(A1, B1), (A3, B3)]] =
    ((f1 split g1) andThen (f2 split g2)) <-> ((f1 andThen f2) split (g1 andThen g2))
}

object SplitLaws {
  def apply[F[_, _]](implicit ev: Split[F]): SplitLaws[F] =
    new SplitLaws[F] { def F: Split[F] = ev }
}
