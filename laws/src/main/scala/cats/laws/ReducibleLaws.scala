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
    fa.reduceMap(f) <-> fa.reduceLeftTo(f)((b, a) => b |+| f(a))

  def reduceRightToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    B: Semigroup[B]
  ): IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).value

  def traverseConsistent[G[_]: Applicative, A, B](fa: F[A], f: A => G[B]): IsEq[G[Unit]] =
    fa.traverse1_(f) <-> fa.traverse_(f)

  def sequenceConsistent[G[_]: Applicative, A](fa: F[G[A]]): IsEq[G[Unit]] =
    fa.sequence1_ <-> fa.sequence_
}

object ReducibleLaws {
  def apply[F[_]](implicit ev: Reducible[F]): ReducibleLaws[F] =
    new ReducibleLaws[F] { def F: Reducible[F] = ev }
}
