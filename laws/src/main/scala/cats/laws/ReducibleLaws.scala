package cats
package laws

import cats.implicits._

trait ReducibleLaws[F[_]] extends FoldableLaws[F] {
  implicit def F: Reducible[F]

  def reduceLeftToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    B: Semigroup[B]
  ): IsEq[B] =
    F.reduceMap(fa)(f) <-> F.reduceLeftTo(fa)(f)((b, a) => b |+| f(a))

  def reduceRightToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    B: Semigroup[B]
  ): IsEq[B] =
    F.reduceMap(fa)(f) <-> F.reduceRightTo(fa)(f)((a, eb) => eb.map(f(a) |+| _)).value

  def traverseConsistent[G[_]: Applicative, A, B](fa: F[A], f: A => G[B]): IsEq[G[Unit]] =
    F.traverse1_(fa)(f) <-> F.traverse_(fa)(f)

  def sequenceConsistent[G[_]: Applicative, A](fa: F[G[A]]): IsEq[G[Unit]] =
    F.sequence1_(fa) <-> fa.sequence_
}

object ReducibleLaws {
  def apply[F[_]](implicit ev: Reducible[F]): ReducibleLaws[F] =
    new ReducibleLaws[F] { def F: Reducible[F] = ev }
}
