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
import compat.scalaVersionSpecific.*

import scala.annotation.nowarn

@suppressUnusedImportWarningForScalaVersionSpecific
trait VectorInstances extends VectorInstances1 {
  implicit def catsKernelStdOrderForVector[A: Order]: Order[Vector[A]] =
    new VectorOrder[A]
  implicit def catsKernelStdMonoidForVector[A]: Monoid[Vector[A]] =
    VectorMonoid[A]
}

private[instances] trait VectorInstances1 extends VectorInstances2 {
  implicit def catsKernelStdPartialOrderForVector[A: PartialOrder]: PartialOrder[Vector[A]] =
    new VectorPartialOrder[A]

  implicit def catsKernelStdHashForVector[A: Hash]: Hash[Vector[A]] =
    new VectorHash[A]
}

private[instances] trait VectorInstances2 {
  implicit def catsKernelStdEqForVector[A: Eq]: Eq[Vector[A]] =
    new VectorEq[A]
}

class VectorOrder[A](implicit ev: Order[A]) extends Order[Vector[A]] {
  def compare(xs: Vector[A], ys: Vector[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class VectorPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Vector[A]] {
  def partialCompare(xs: Vector[A], ys: Vector[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class VectorHash[A](implicit ev: Hash[A]) extends VectorEq[A] with Hash[Vector[A]] {
  def hash(xs: Vector[A]): Int = StaticMethods.orderedHash(xs)
}

class VectorEq[A](implicit ev: Eq[A]) extends Eq[Vector[A]] {
  def eqv(xs: Vector[A], ys: Vector[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

@deprecated("Use VectorMonoid.apply, which does not allocate a new instance", "2.9.0")
class VectorMonoid[A] extends Monoid[Vector[A]] {
  def empty: Vector[A] = Vector.empty
  def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  override def combineN(x: Vector[A], n: Int): Vector[A] =
    StaticMethods.combineNIterable(Vector.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Vector[A]]): Vector[A] =
    StaticMethods.combineAllIterable(Vector.newBuilder[A], xs)
}

object VectorMonoid {
  @nowarn("msg=deprecated")
  private[this] val singleton: Monoid[Vector[Any]] = new VectorMonoid[Any]

  def apply[A]: Monoid[Vector[A]] = singleton.asInstanceOf[Monoid[Vector[A]]]
}
