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

import scala.annotation.nowarn

trait LazyListInstances extends LazyListInstances1 {
  implicit def catsKernelStdOrderForLazyList[A: Order]: Order[LazyList[A]] =
    new LazyListOrder[A]

  implicit def catsKernelStdMonoidForLazyList[A]: Monoid[LazyList[A]] =
    LazyListMonoid[A]
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

@deprecated("Use LazyListMonoid.apply, which does not allocate a new instance", "2.9.0")
class LazyListMonoid[A] extends Monoid[LazyList[A]] {
  def empty: LazyList[A] = LazyList.empty
  def combine(x: LazyList[A], y: LazyList[A]): LazyList[A] = x ++ y
  override def combineN(x: LazyList[A], n: Int): LazyList[A] =
    StaticMethods.combineNIterable(LazyList.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[LazyList[A]]): LazyList[A] =
    StaticMethods.combineAllIterable(LazyList.newBuilder[A], xs)
}

object LazyListMonoid {
  @nowarn("msg=deprecated")
  private[this] val singleton: Monoid[LazyList[Any]] = new LazyListMonoid[Any]
  def apply[A]: Monoid[LazyList[A]] = singleton.asInstanceOf[Monoid[LazyList[A]]]
}
