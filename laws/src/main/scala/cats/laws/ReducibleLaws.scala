package cats
package laws

import cats.implicits._

trait ReducibleLaws[F[_]] extends FoldableLaws[F] {
  implicit def F: Reducible[F]

  def reduceLeftToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceLeftTo(f)((b, a) => b |+| f(a))

  def reduceRightToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).value

  def reduceRightToConsistentWithReduceRightToOption[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[Option[B]] =
    fa.reduceRightToOption(f)((a, eb) => eb.map(f(a) |+| _)).value <->
      fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).map(Option(_)).value

  def reduceRightConsistentWithReduceRightOption[A](fa: F[A], f: (A, A) => A): IsEq[Option[A]] =
    fa.reduceRight((a1, e2) => Now(f(a1, e2.value))).map(Option(_)).value <->
      fa.reduceRightOption((a1, e2) => Now(f(a1, e2.value))).value

  def reduceReduceLeftConsistent[B](fa: F[B])(implicit B: Semigroup[B]): IsEq[B] =
    fa.reduce <-> fa.reduceLeft(B.combine)

  def traverseConsistent[G[_]: Applicative, A, B](fa: F[A], f: A => G[B]): IsEq[G[Unit]] =
    fa.nonEmptyTraverse_(f) <-> fa.traverse_(f)

  def sequenceConsistent[G[_]: Applicative, A](fa: F[G[A]]): IsEq[G[Unit]] =
    fa.nonEmptySequence_ <-> fa.sequence_

  def sizeConsistent[A](fa: F[A]): IsEq[Long] =
    fa.size <-> fa.reduceMap(_ => 1L)
}

object ReducibleLaws {
  def apply[F[_]](implicit ev: Reducible[F]): ReducibleLaws[F] =
    new ReducibleLaws[F] { def F: Reducible[F] = ev }
}
