package cats.kernel
package instances
import compat.scalaVersionSpecific._

trait StreamInstances extends StreamInstances1 {
  implicit def catsKernelStdOrderForStream[A: Order]: Order[LazyList[A]] =
    new StreamOrder[A]
  implicit def catsKernelStdMonoidForStream[A]: Monoid[LazyList[A]] =
    new StreamMonoid[A]
}

trait StreamInstances1 extends StreamInstances2 {
  implicit def catsKernelStdPartialOrderForStream[A: PartialOrder]: PartialOrder[LazyList[A]] =
    new StreamPartialOrder[A]

  implicit def catsKernelStdHashForStream[A: Hash]: Hash[LazyList[A]] =
    new StreamHash[A]
}

trait StreamInstances2 {
  implicit def catsKernelStdEqForStream[A: Eq]: Eq[LazyList[A]] =
    new StreamEq[A]
}

class StreamOrder[A](implicit ev: Order[A]) extends Order[LazyList[A]] {
  def compare(xs: LazyList[A], ys: LazyList[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class StreamPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[LazyList[A]] {
  def partialCompare(xs: LazyList[A], ys: LazyList[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class StreamHash[A](implicit ev: Hash[A]) extends StreamEq[A]()(ev) with Hash[LazyList[A]] {
  def hash(xs: LazyList[A]): Int = StaticMethods.orderedHash(xs)
}

class StreamEq[A](implicit ev: Eq[A]) extends Eq[LazyList[A]] {
  def eqv(xs: LazyList[A], ys: LazyList[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class StreamMonoid[A] extends Monoid[LazyList[A]] {
  def empty: LazyList[A] = LazyList.empty
  def combine(x: LazyList[A], y: LazyList[A]): LazyList[A] = x ++ y
  override def combineN(x: LazyList[A], n: Int): LazyList[A] =
    StaticMethods.combineNIterable(LazyList.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[LazyList[A]]): LazyList[A] =
    StaticMethods.combineAllIterable(LazyList.newBuilder[A], xs)
}
