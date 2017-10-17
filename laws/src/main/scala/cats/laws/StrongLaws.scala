package cats
package laws

import cats.arrow.Strong
import cats.syntax.profunctor._
import cats.syntax.strong._
import cats.instances.function._

/**
 * Laws that must be obeyed by any `cats.functor.Strong`.
 */
trait StrongLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit override def F: Strong[F]

  def strongFirstDistributivity[A0, A1, B1, B2, C](fab: F[A1, B1], f: A0 => A1, g: B1 => B2): IsEq[F[(A0, C), (B2, C)]] =
    fab.dimap(f)(g).first[C] <-> fab.first[C].dimap(f.first[C])(g.first[C])

  def strongSecondDistributivity[A0, A1, B1, B2, C](fab: F[A1, B1], f: A0 => A1, g: B1 => B2): IsEq[F[(C, A0), (C, B2)]] =
    fab.dimap(f)(g).second[C] <-> fab.second[C].dimap(f.second[C])(g.second[C])
}

object StrongLaws {
  def apply[F[_, _]](implicit ev: Strong[F]): StrongLaws[F] =
    new StrongLaws[F] { def F: Strong[F] = ev }
}
