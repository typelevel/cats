package cats.kernel
package instances
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait StreamInstances extends StreamInstances1 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
  implicit def catsKernelStdOrderForStream[A: Order]: Order[Stream[A]] =
    new StreamOrder[A]
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
  implicit def catsKernelStdMonoidForStream[A]: Monoid[Stream[A]] =
    new StreamMonoid[A]
}

trait StreamInstances1 extends StreamInstances2 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
  implicit def catsKernelStdPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    new StreamPartialOrder[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
  implicit def catsKernelStdHashForStream[A: Hash]: Hash[Stream[A]] =
    new StreamHash[A]
}

trait StreamInstances2 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
  implicit def catsKernelStdEqForStream[A: Eq]: Eq[Stream[A]] =
    new StreamEq[A]
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
class StreamOrder[A](implicit ev: Order[A]) extends Order[Stream[A]] {
  def compare(xs: Stream[A], ys: Stream[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
class StreamPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Stream[A]] {
  def partialCompare(xs: Stream[A], ys: Stream[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
class StreamHash[A](implicit ev: Hash[A]) extends StreamEq[A]()(ev) with Hash[Stream[A]] {
  def hash(xs: Stream[A]): Int = StaticMethods.orderedHash(xs)
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
class StreamEq[A](implicit ev: Eq[A]) extends Eq[Stream[A]] {
  def eqv(xs: Stream[A], ys: Stream[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.lazyList")
class StreamMonoid[A] extends Monoid[Stream[A]] {
  def empty: Stream[A] = Stream.empty
  def combine(x: Stream[A], y: Stream[A]): Stream[A] = x ++ y
  override def combineN(x: Stream[A], n: Int): Stream[A] =
    StaticMethods.combineNIterable(Stream.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Stream[A]]): Stream[A] =
    StaticMethods.combineAllIterable(Stream.newBuilder[A], xs)
}
