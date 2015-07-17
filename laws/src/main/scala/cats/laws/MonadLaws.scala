package cats
package laws

import cats.data.Kleisli
import cats.syntax.flatMap._

/**
 * Laws that must be obeyed by any `Monad`.
 */
trait MonadLaws[F[_]] extends ApplicativeLaws[F] with FlatMapLaws[F] {
  implicit override def F: Monad[F]

  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(F.pure) <-> fa

  /**
   * `pure` is the left identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadLeftIdentity]].
   */
  def kleisliLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    (Kleisli(F.pure[A]) andThen Kleisli(f)).run(a) <-> f(a)

  /**
   * `pure` is the right identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadRightIdentity]].
   */
  def kleisliRightIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    (Kleisli(f) andThen Kleisli(F.pure[B])).run(a) <-> f(a)
}

object MonadLaws {
  def apply[F[_]](implicit ev: Monad[F]): MonadLaws[F] =
    new MonadLaws[F] { def F: Monad[F] = ev }
}
