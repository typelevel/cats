package cats
package laws

import cats.data.INothing
import cats.syntax.all._

trait InvariantSemiringalLaws[F[_]] extends InvariantMonoidalLaws[F] with InvariantChoosableLaws[F] {
  implicit override def I: InvariantSemiringal[F]
  override def F: InvariantSemiringal[F] = I

  def semiringalRightAbsorption[A](fa: F[A]): IsEq[F[INothing]] =
    I.product[A, INothing](fa, I.zero).imap(_._2)(identity) <-> I.zero
}

object InvariantSemiringalLaws {
  def apply[F[_]](implicit ev: InvariantSemiringal[F]): InvariantSemiringalLaws[F] =
    new InvariantSemiringalLaws[F] { def I: InvariantSemiringal[F] = ev }
}
