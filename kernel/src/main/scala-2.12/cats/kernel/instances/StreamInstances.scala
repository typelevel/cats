/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.kernel
package instances

trait StreamInstances extends StreamInstances1 {
  implicit def catsKernelStdOrderForStream[A: Order]: Order[Stream[A]] =
    new StreamOrder[A]
  implicit def catsKernelStdMonoidForStream[A]: Monoid[Stream[A]] =
    new StreamMonoid[A]
}

private[instances] trait StreamInstances1 extends StreamInstances2 {
  implicit def catsKernelStdPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    new StreamPartialOrder[A]

  implicit def catsKernelStdHashForStream[A: Hash]: Hash[Stream[A]] =
    new StreamHash[A]
}

private[instances] trait StreamInstances2 {
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

class StreamHash[A](implicit ev: Hash[A]) extends StreamEq[A](ev) with Hash[Stream[A]] {
  def hash(xs: Stream[A]): Int = StaticMethods.orderedHash(xs)
}

class StreamEq[A](implicit ev: Eq[A]) extends Eq[Stream[A]] {
  def eqv(xs: Stream[A], ys: Stream[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class StreamMonoid[A] extends Monoid[Stream[A]] {
  def empty: Stream[A] = Stream.empty
  def combine(x: Stream[A], y: Stream[A]): Stream[A] = x ++ y
  override def combineN(x: Stream[A], n: Int): Stream[A] =
    StaticMethods.combineNIterable(Stream.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[Stream[A]]): Stream[A] =
    StaticMethods.combineAllIterable(Stream.newBuilder[A], xs)
}
