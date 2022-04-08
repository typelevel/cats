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

package cats.kernel.laws

import cats.kernel.{Eq, PartialOrder}
import cats.kernel.instances.option._

trait PartialOrderLaws[A] extends EqLaws[A] {
  implicit override def E: PartialOrder[A]

  def reflexivityLt(x: A): IsEq[Boolean] =
    E.lteqv(x, x) <-> true

  def reflexivityGt(x: A): IsEq[Boolean] =
    E.gteqv(x, x) <-> true

  def antisymmetry(x: A, y: A): IsEq[Boolean] =
    (!(E.lteqv(x, y) && E.lteqv(y, x)) || E.eqv(x, y)) <-> true

  def transitivity(x: A, y: A, z: A): IsEq[Boolean] =
    (!(E.lteqv(x, y) && E.lteqv(y, z)) || E.lteqv(x, z)) <-> true

  def gteqv(x: A, y: A): IsEq[Boolean] =
    E.lteqv(x, y) <-> E.gteqv(y, x)

  def lt(x: A, y: A): IsEq[Boolean] =
    E.lt(x, y) <-> (E.lteqv(x, y) && E.neqv(x, y))

  def gt(x: A, y: A): IsEq[Boolean] =
    E.lt(x, y) <-> E.gt(y, x)

  def partialCompare(x: A, y: A): IsEq[Boolean] = {
    val c = E.partialCompare(x, y)
    (((c < 0) == E.lt(x, y)) && ((c == 0) == E.eqv(x, y)) && ((c > 0) == E.gt(x, y))) <-> true
  }

  def pmin(x: A, y: A): IsEq[Boolean] = {
    val c = E.partialCompare(x, y)
    val m = E.pmin(x, y)
    if (c < 0) Eq[Option[A]].eqv(m, Option(x)) <-> true
    else if (c == 0) (Eq[Option[A]].eqv(m, Option(x)) && Eq[Option[A]].eqv(m, Option(y))) <-> true
    else if (c > 0) Eq[Option[A]].eqv(m, Option(y)) <-> true
    else Eq[Option[A]].eqv(m, None) <-> true
  }

  def pmax(x: A, y: A): IsEq[Boolean] = {
    val c = E.partialCompare(x, y)
    val m = E.pmax(x, y)
    if (c < 0) Eq[Option[A]].eqv(m, Option(y)) <-> true
    else if (c == 0) (Eq[Option[A]].eqv(m, Option(x)) && Eq[Option[A]].eqv(m, Option(y))) <-> true
    else if (c > 0) Eq[Option[A]].eqv(m, Option(x)) <-> true
    else Eq[Option[A]].eqv(m, None) <-> true
  }

}

object PartialOrderLaws {
  def apply[A](implicit ev: PartialOrder[A]): PartialOrderLaws[A] =
    new PartialOrderLaws[A] { def E: PartialOrder[A] = ev }
}
