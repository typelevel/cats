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

import scala.reflect.ClassTag
import compat.scalaVersionSpecific.*

import scala.annotation.nowarn

@suppressUnusedImportWarningForScalaVersionSpecific
trait IArrayInstances extends IArrayInstances1 {
  implicit def catsKernelStdOrderForIArray[A: Order: ClassTag]: Order[IArray[A]] =
    new IArrayOrder[A]
  implicit def catsKernelStdMonoidForIArray[A: ClassTag]: Monoid[IArray[A]] =
    IArrayMonoid[A]
}

private[instances] trait IArrayInstances1 extends IArrayInstances2 {
  implicit def catsKernelStdPartialOrderForIArray[A: PartialOrder]: PartialOrder[IArray[A]] =
    new IArrayPartialOrder[A]

  implicit def catsKernelStdHashForIArray[A: Hash]: Hash[IArray[A]] =
    new IArrayHash[A]
}

private[instances] trait IArrayInstances2 {
  implicit def catsKernelStdEqForIArray[A: Eq]: Eq[IArray[A]] =
    new IArrayEq[A]
}

class IArrayOrder[A](implicit ev: Order[A]) extends Order[IArray[A]] {
  def compare(xs: IArray[A], ys: IArray[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class IArrayPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[IArray[A]] {
  def partialCompare(xs: IArray[A], ys: IArray[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class IArrayHash[A](implicit ev: Hash[A]) extends IArrayEq[A] with Hash[IArray[A]] {
  def hash(xs: IArray[A]): Int = StaticMethods.orderedHash(xs)
}

class IArrayEq[A](implicit ev: Eq[A]) extends Eq[IArray[A]] {
  def eqv(xs: IArray[A], ys: IArray[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

private[instances] class IArrayMonoid[A](implicit ev: ClassTag[A]) extends Monoid[IArray[A]] {
  def empty: IArray[A] = IArray.empty
  def combine(x: IArray[A], y: IArray[A]): IArray[A] = x ++ y

  override def combineN(x: IArray[A], n: Int): IArray[A] =
    val builder = IArray.newBuilder[A]
    var i = n
    while (i > 0) { builder ++= x.iterator; i -= 1 }
    builder.result()

  override def combineAll(xs: IterableOnce[IArray[A]]): IArray[A] =
    val builder = IArray.newBuilder[A]
    xs.foreach(builder ++= _.iterator)
    builder.result()
}

object IArrayMonoid {
  def apply[A: ClassTag]: Monoid[IArray[A]] = new IArrayMonoid[A]
}
