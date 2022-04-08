/*
 * Copyright (c) 2022 Typelevel
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

trait SetInstances extends SetInstances1 {
  implicit def catsKernelStdHashForSet[A]: Hash[Set[A]] =
    new SetHash[A]
}

private[instances] trait SetInstances1 {
  implicit def catsKernelStdPartialOrderForSet[A]: PartialOrder[Set[A]] =
    new SetPartialOrder[A]

  implicit def catsKernelStdSemilatticeForSet[A]: BoundedSemilattice[Set[A]] =
    new SetSemilattice[A]
}

class SetPartialOrder[A] extends PartialOrder[Set[A]] {
  def partialCompare(x: Set[A], y: Set[A]): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  override def eqv(x: Set[A], y: Set[A]): Boolean = x == y
}

class SetHash[A] extends Hash[Set[A]] {
  // Does not require a Hash on elements: Scala sets must use the universal `hashCode`.
  def hash(x: Set[A]): Int = x.hashCode()

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  def eqv(x: Set[A], y: Set[A]): Boolean = x == y
}

class SetSemilattice[A] extends BoundedSemilattice[Set[A]] {
  def empty: Set[A] = Set.empty
  def combine(x: Set[A], y: Set[A]): Set[A] = x | y
}
