package cats
package laws

import cats.syntax.all._

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {
  implicit def F0: Alternative[F]
  implicit def F: Applicative[F] = F0.applicativeInstance
  implicit def algebra[A]: Monoid[F[A]] = F0.algebra[A]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (ff ap F0.empty[A]) <-> F0.empty[B]

  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2) map f) <-> ((fa map f) |+| (fa2 map f))

  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff |+| fg) ap fa) <-> ((ff ap fa) |+| (fg ap fa))

}

object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F0: Alternative[F] = ev }
}
