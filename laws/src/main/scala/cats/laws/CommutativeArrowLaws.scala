package cats
package laws

import cats.arrow.CommutativeArrow
import cats.syntax.compose._
import cats.syntax.strong._

/**
 * Reference: "Causal Commutative Arrows", Journal of Functional Programming
 *  Figure 4.
 */
trait CommutativeArrowLaws[F[_, _]] extends ArrowLaws[F] {
  implicit override def F: CommutativeArrow[F]

  def arrowCommutative[A, B, C, D](f: F[A, B], g: F[C, D]): IsEq[F[(A, C), (B, D)]] =
    (f.first[C] >>> g.second[B]) <-> (g.second[A] >>> f.first[D])

}

object CommutativeArrowLaws {
  def apply[F[_, _]](implicit ev: CommutativeArrow[F]): CommutativeArrowLaws[F] =
    new CommutativeArrowLaws[F] { def F: CommutativeArrow[F] = ev }
}
