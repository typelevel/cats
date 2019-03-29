package cats
package laws

import cats.syntax.all._

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {
  implicit override def F: Alternative[F]
  implicit def algebra[A]: Monoid[F[A]] = F.algebra[A]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (ff.ap(F.empty[A])) <-> F.empty[B]

  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2).map(f)) <-> ((fa.map(f)) |+| (fa2.map(f)))

  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff |+| fg).ap(fa)) <-> ((ff.ap(fa)) |+| (fg.ap(fa)))

}

object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F: Alternative[F] = ev }
}
