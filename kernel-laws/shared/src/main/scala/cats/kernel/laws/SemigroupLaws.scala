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

package cats.kernel.laws

import cats.kernel.Semigroup

trait SemigroupLaws[A] {
  implicit def S: Semigroup[A]

  def semigroupAssociative(x: A, y: A, z: A): IsEq[A] =
    S.combine(S.combine(x, y), z) <-> S.combine(x, S.combine(y, z))

  def repeat1(a: A): IsEq[A] =
    S.combineN(a, 1) <-> a

  def repeat2(a: A): IsEq[A] =
    S.combineN(a, 2) <-> S.combine(a, a)

  def combineAllOption(xs: Vector[A]): IsEq[Option[A]] =
    S.combineAllOption(xs) <-> xs.reduceOption(S.combine)

  def reverseReverses(a: A, b: A): IsEq[A] =
    S.combine(a, b) <-> S.reverse.combine(b, a)

  def reverseRepeat1(a: A): IsEq[A] = {
    val rev = S.reverse
    rev.combineN(a, 1) <-> a
  }

  def reverseRepeat2(a: A): IsEq[A] = {
    val rev = S.reverse
    rev.combineN(a, 2) <-> rev.combine(a, a)
  }

  def reverseCombineAllOption(xs: Vector[A]): IsEq[Option[A]] = {
    val rev = S.reverse
    rev.combineAllOption(xs) <-> xs.reduceOption(rev.combine)
  }

  def intercalateIntercalates(a: A, m: A, b: A): IsEq[A] =
    S.combine(a, S.combine(m, b)) <-> S.intercalate(m).combine(a, b)

  def intercalateRepeat1(m: A, a: A): IsEq[A] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineN(a, 1) <-> a
  }

  def intercalateRepeat2(m: A, a: A): IsEq[A] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineN(a, 2) <-> withMiddle.combine(a, a)
  }

  def intercalateCombineAllOption(m: A, xs: Vector[A]): IsEq[Option[A]] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineAllOption(xs) <-> xs.reduceOption(withMiddle.combine)
  }
}

object SemigroupLaws {
  def apply[A](implicit ev: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] { def S: Semigroup[A] = ev }
}
