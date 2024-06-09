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

import cats.kernel.{Eq, Monoid}

trait MonoidLaws[A] extends SemigroupLaws[A] {
  implicit override def S: Monoid[A]

  def leftIdentity(x: A): IsEq[A] =
    S.combine(S.empty, x) <-> x

  def rightIdentity(x: A): IsEq[A] =
    S.combine(x, S.empty) <-> x

  def repeat0(x: A): IsEq[A] =
    S.combineN(x, 0) <-> S.empty

  def collect0: IsEq[A] =
    S.combineAll(Nil) <-> S.empty

  def combineAll(xs: Vector[A]): IsEq[A] =
    S.combineAll(xs) <-> (S.empty +: xs).reduce(S.combine)

  def isId(x: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(x, S.empty) <-> S.isEmpty(x)(eqv)

  @deprecated("use `collect0` without parameters", "2.12.1")
  def collect0(x: A): IsEq[A] = collect0
}

object MonoidLaws {
  def apply[A](implicit ev: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def S: Monoid[A] = ev }
}
