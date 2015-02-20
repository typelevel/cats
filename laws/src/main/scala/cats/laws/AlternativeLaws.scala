package cats.laws

import cats.Alternative

import cats.syntax.all._

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {
  implicit override def F: Alternative[F]

  def rightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    F.empty[A].apply(ff) <-> F.empty[B]

  def leftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    F.combine(fa, fa2).map(f) <-> F.combine(fa map f, fa2 map f)

  def rightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    fa.apply(F.combine(ff,fg)) <-> F.combine(fa.apply(ff), fa.apply(fg))


}

object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F = ev }
}
