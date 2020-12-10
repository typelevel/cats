package cats
package laws

import cats.data.Kleisli
import cats.implicits._

/**
 * Laws that must be obeyed by any `Monad`.
 */
trait MonadLaws[F[_]] extends SelectiveLaws[F] with FlatMapLaws[F] {
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
    Kleisli(F.pure[A]).andThen(Kleisli(f)).run(a) <-> f(a)

  /**
   * `pure` is the right identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadRightIdentity]].
   */
  def kleisliRightIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    Kleisli(f).andThen(Kleisli(F.pure[B])).run(a) <-> f(a)

  /**
   * Make sure that map and flatMap are consistent.
   */
  def mapFlatMapCoherence[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.flatMap(a => F.pure(f(a))) <-> fa.map(f)

  lazy val tailRecMStackSafety: IsEq[F[Int]] = {
    val n = 50000
    val res = F.tailRecM(0)(i => F.pure(if (i < n) Either.left(i + 1) else Either.right(i)))
    res <-> F.pure(n)
  }

  def selectRigidity[A, B](fab: F[Either[A, B]], ff: F[A => B]): IsEq[F[B]] = {
    def selectM[G[_]: Monad](gab: G[Either[A, B]])(gf: G[A => B]) =
      gab.flatMap {
        case Left(a)  => gf.map(_(a))
        case Right(b) => b.pure[G]
      }
    F.select(fab)(ff) <-> selectM(fab)(ff)
  }
}

object MonadLaws {
  def apply[F[_]](implicit ev: Monad[F]): MonadLaws[F] =
    new MonadLaws[F] { def F: Monad[F] = ev }
}
