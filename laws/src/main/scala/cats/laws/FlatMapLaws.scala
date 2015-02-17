package cats.laws

import cats.FlatMap
import cats.data.Kleisli
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any [[FlatMap]].
 */
trait FlatMapLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: FlatMap[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))

  def flatMapConsistentApply[A, B](fa: F[A], fab: F[A => B]): IsEq[F[B]] =
    fa.apply(fab) <-> fab.flatMap(f => fa.map(f))

  /**
   * The composition of [[cats.data.Kleisli]] arrows is associative. This is
   * analogous to the associativity law of [[FlatMap.flatMap]].
   */
  def kleisliAssociativity[A, B, C, D](f: A => F[B], g: B => F[C], h: C => F[D], a: A): IsEq[F[D]] = {
    val (kf, kg, kh) = (Kleisli(f), Kleisli(g), Kleisli(h))
    (kh compose (kg compose kf)).run(a) <-> ((kh compose kg) compose kf).run(a)
  }
}

object FlatMapLaws {
  def apply[F[_]](implicit ev: FlatMap[F]): FlatMapLaws[F] =
    new FlatMapLaws[F] { def F = ev }
}
