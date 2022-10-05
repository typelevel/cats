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

import scala.collection.immutable.Queue
import compat.scalaVersionSpecific._

import scala.annotation.nowarn

@suppressUnusedImportWarningForScalaVersionSpecific
trait QueueInstances extends QueueInstances1 {
  implicit def catsKernelStdOrderForQueue[A: Order]: Order[Queue[A]] =
    new QueueOrder[A]
  implicit def catsKernelStdMonoidForQueue[A]: Monoid[Queue[A]] = QueueMonoid[A]
}

private[instances] trait QueueInstances1 extends QueueInstances2 {
  implicit def catsKernelStdPartialOrderForQueue[A: PartialOrder]: PartialOrder[Queue[A]] =
    new QueuePartialOrder[A]

  implicit def catsKernelStdHashForQueue[A: Hash]: Hash[Queue[A]] =
    new QueueHash[A]
}

private[instances] trait QueueInstances2 {
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

@deprecated("Use QueueMonoid.apply, which does not allocate a new instance", "2.9.0")
class QueueMonoid[A] extends Monoid[Queue[A]] {
  def empty: Queue[A] = Queue.empty[A]
  def combine(x: Queue[A], y: Queue[A]): Queue[A] = x ++ y

  override def combineN(x: Queue[A], n: Int): Queue[A] =
    StaticMethods.combineNIterable(Queue.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Queue[A]]): Queue[A] =
    StaticMethods.combineAllIterable(Queue.newBuilder[A], xs)
}

object QueueMonoid {
  @nowarn("msg=deprecated")
  private[this] val singleton: Monoid[Queue[Any]] = new QueueMonoid[Any]

  def apply[A]: Monoid[Queue[A]] = singleton.asInstanceOf[Monoid[Queue[A]]]
}
