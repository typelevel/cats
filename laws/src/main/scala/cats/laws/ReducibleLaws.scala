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
}

object ReducibleLaws {
  def apply[F[_]](implicit ev: Reducible[F]): ReducibleLaws[F] =
    new ReducibleLaws[F] { def F: Reducible[F] = ev }
}
