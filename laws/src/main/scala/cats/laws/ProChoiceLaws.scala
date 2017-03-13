package cats
package laws

import cats.arrow.ProChoice
import cats.instances.function._
import cats.syntax.all._

/**
 * Laws that must be obeyed by any [[cats.functor.ProChoice]].
 */
trait ProChoiceLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit override def F: ProChoice[F]

  def proChoiceLeftDistributivity[A0, A1, B1, B2, C](fab: F[A1, B1], f: A0 => A1, g: B1 => B2): IsEq[F[Either[A0, C], Either[B2, C]]] =
    fab.dimap(f)(g).left[C] <-> fab.left[C].dimap(f.left[C])(g.left[C])

  def proChoiceRightDistributivity[A0, A1, B1, B2, C](fab: F[A1, B1], f: A0 => A1, g: B1 => B2): IsEq[F[Either[C, A0], Either[C, B2]]] =
    fab.dimap(f)(g).right[C] <-> fab.right[C].dimap(f.right[C])(g.right[C])
}

object ProChoiceLaws {
  def apply[F[_, _]](implicit ev: ProChoice[F]): ProChoiceLaws[F] =
    new ProChoiceLaws[F] { def F: ProChoice[F] = ev }
}
