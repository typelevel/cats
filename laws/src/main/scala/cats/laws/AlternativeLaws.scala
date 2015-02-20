package cats.laws

import cats.Alternative

import cats.syntax.all._

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {
  implicit override def F: Alternative[F]
  implicit def algebra[A] = F.algebra[A]

  def rightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (F.empty[A] apply ff) <-> F.empty[B]

  def leftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2) map f) <-> ((fa map f) |+| (fa2 map f))

  def rightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    (fa apply (ff |+| fg)) <-> ((fa apply ff) |+| (fa apply fg))

}

object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F = ev }
}
