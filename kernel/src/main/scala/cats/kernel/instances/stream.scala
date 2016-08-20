package cats.kernel
package instances

package object stream extends StreamInstances

trait StreamInstances extends StreamInstances1 {
  implicit def catsKernelStdOrderForStream[A: Order]: Order[Stream[A]] =
    new StreamOrder[A]
  implicit def catsKernelStdMonoidForStream[A]: Monoid[Stream[A]] =
    new StreamMonoid[A]
}

trait StreamInstances1 extends StreamInstances2 {
  implicit def catsKernelStdPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    new StreamPartialOrder[A]
}

trait StreamInstances2 {
  implicit def catsKernelStdEqForStream[A: Eq]: Eq[Stream[A]] =
    new StreamEq[A]
}

class StreamOrder[A](implicit ev: Order[A]) extends Order[Stream[A]] {
  def compare(xs: Stream[A], ys: Stream[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class StreamPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Stream[A]] {
  def partialCompare(xs: Stream[A], ys: Stream[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class StreamEq[A](implicit ev: Eq[A]) extends Eq[Stream[A]] {
  def eqv(xs: Stream[A], ys: Stream[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class StreamMonoid[A] extends Monoid[Stream[A]] {
  def empty: Stream[A] = Stream.empty
  def combine(x: Stream[A], y: Stream[A]): Stream[A] = x ++ y

  override def combineN(x: Stream[A], n: Int): Stream[A] = {
    val buf = Stream.newBuilder[A]
    var i = n
    while (i > 0) {
      buf ++= x
      i -= 1
    }
    buf.result
  }

  override def combineAll(xs: TraversableOnce[Stream[A]]): Stream[A] = {
    val buf = Stream.newBuilder[A]
    xs.foreach(buf ++= _)
    buf.result
  }
}
