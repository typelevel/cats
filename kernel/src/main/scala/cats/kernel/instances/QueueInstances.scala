package cats.kernel
package instances

import scala.collection.immutable.Queue
import compat.scalaVersionSpecific._

trait QueueInstances extends QueueInstances1 {
  implicit def catsKernelStdOrderForQueue[A: Order]: Order[Queue[A]] =
    new QueueOrder[A]
  implicit def catsKernelStdMonoidForQueue[A]: Monoid[Queue[A]] =
    new QueueMonoid[A]
}

trait QueueInstances1 extends QueueInstances2 {
  implicit def catsKernelStdPartialOrderForQueue[A: PartialOrder]: PartialOrder[Queue[A]] =
    new QueuePartialOrder[A]

  implicit def catsKernelStdHashForQueue[A: Hash]: Hash[Queue[A]] =
    new QueueHash[A]
}

trait QueueInstances2 {
  implicit def catsKernelStdEqForQueue[A: Eq]: Eq[Queue[A]] =
    new QueueEq[A]
}

class QueueOrder[A](implicit ev: Order[A]) extends Order[Queue[A]] {
  def compare(xs: Queue[A], ys: Queue[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class QueueHash[A](implicit ev: Hash[A]) extends QueueEq[A] with Hash[Queue[A]] {
  def hash(x: Queue[A]): Int = StaticMethods.orderedHash(x)
}

class QueuePartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Queue[A]] {
  def partialCompare(xs: Queue[A], ys: Queue[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class QueueEq[A](implicit ev: Eq[A]) extends Eq[Queue[A]] {
  def eqv(xs: Queue[A], ys: Queue[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class QueueMonoid[A] extends Monoid[Queue[A]] {
  def empty: Queue[A] = Queue.empty[A]
  def combine(x: Queue[A], y: Queue[A]): Queue[A] = x ++ y

  override def combineN(x: Queue[A], n: Int): Queue[A] =
    StaticMethods.combineNIterable(Queue.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Queue[A]]): Queue[A] =
    StaticMethods.combineAllIterable(Queue.newBuilder[A], xs)
}
