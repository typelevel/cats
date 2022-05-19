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

import compat.scalaVersionSpecific._
import scala.collection.immutable.Seq

@suppressUnusedImportWarningForScalaVersionSpecific
trait SeqInstances extends SeqInstances1 {
  implicit def catsKernelStdOrderForSeq[A: Order]: Order[Seq[A]] =
    new SeqOrder[A]
  implicit def catsKernelStdMonoidForSeq[A]: Monoid[Seq[A]] =
    new SeqMonoid[A]
}

private[instances] trait SeqInstances1 extends SeqInstances2 {
  implicit def catsKernelStdPartialOrderForSeq[A: PartialOrder]: PartialOrder[Seq[A]] =
    new SeqPartialOrder[A]

  implicit def catsKernelStdHashForSeq[A: Hash]: Hash[Seq[A]] =
    new SeqHash[A]
}

private[instances] trait SeqInstances2 {
  implicit def catsKernelStdEqForSeq[A: Eq]: Eq[Seq[A]] =
    new SeqEq[A]
}

class SeqOrder[A](implicit ev: Order[A]) extends Order[Seq[A]] {
  def compare(xs: Seq[A], ys: Seq[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class SeqPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Seq[A]] {
  def partialCompare(xs: Seq[A], ys: Seq[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class SeqHash[A](implicit ev: Hash[A]) extends SeqEq[A] with Hash[Seq[A]] {
  def hash(xs: Seq[A]): Int = StaticMethods.orderedHash(xs)
}

class SeqEq[A](implicit ev: Eq[A]) extends Eq[Seq[A]] {
  def eqv(xs: Seq[A], ys: Seq[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class SeqMonoid[A] extends Monoid[Seq[A]] {
  def empty: Seq[A] = Seq.empty
  def combine(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

  override def combineN(x: Seq[A], n: Int): Seq[A] =
    StaticMethods.combineNIterable(Seq.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Seq[A]]): Seq[A] =
    StaticMethods.combineAllIterable(Seq.newBuilder[A], xs)
}
