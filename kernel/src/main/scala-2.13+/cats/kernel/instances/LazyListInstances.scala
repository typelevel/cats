package cats.kernel
package instances

trait LazyListInstances extends LazyListInstances1 {
  implicit def catsKernelStdOrderForLazyList[A: Order]: Order[LazyList[A]] =
    new LazyListOrder[A]
  implicit def catsKernelStdMonoidForLazyList[A]: Monoid[LazyList[A]] =
    new LazyListMonoid[A]
}

private[instances] trait LazyListInstances1 extends LazyListInstances2 {
  implicit def catsKernelStdPartialOrderForLazyList[A: PartialOrder]: PartialOrder[LazyList[A]] =
    new LazyListPartialOrder[A]

  implicit def catsKernelStdHashForLazyList[A: Hash]: Hash[LazyList[A]] =
    new LazyListHash[A]
}

private[instances] trait LazyListInstances2 {
  implicit def catsKernelStdEqForLazyList[A: Eq]: Eq[LazyList[A]] =
    new LazyListEq[A]
}

class LazyListOrder[A](implicit ev: Order[A]) extends Order[LazyList[A]] {
  def compare(xs: LazyList[A], ys: LazyList[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class LazyListPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[LazyList[A]] {
  def partialCompare(xs: LazyList[A], ys: LazyList[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class LazyListHash[A](implicit ev: Hash[A]) extends LazyListEq[A]()(ev) with Hash[LazyList[A]] {
  def hash(xs: LazyList[A]): Int = StaticMethods.orderedHash(xs)
}

class LazyListEq[A](implicit ev: Eq[A]) extends Eq[LazyList[A]] {
  def eqv(xs: LazyList[A], ys: LazyList[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class LazyListMonoid[A] extends Monoid[LazyList[A]] {
  def empty: LazyList[A] = LazyList.empty
  def combine(x: LazyList[A], y: LazyList[A]): LazyList[A] = x ++ y
  override def combineN(x: LazyList[A], n: Int): LazyList[A] =
    StaticMethods.combineNIterable(LazyList.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[LazyList[A]]): LazyList[A] =
    StaticMethods.combineAllIterable(LazyList.newBuilder[A], xs)
}
