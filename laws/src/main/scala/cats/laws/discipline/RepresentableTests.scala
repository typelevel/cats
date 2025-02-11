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

package cats.laws.discipline

import cats.{Eq, Representable}
import cats.laws.RepresentableLaws
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.*

trait RepresentableTests[F[_], R] extends Laws {

  val laws: RepresentableLaws[F, R]

  def representable[A](implicit
    ArbA: Arbitrary[A],
    ArbFA: Arbitrary[F[A]],
    ArbRep: Arbitrary[R],
    ArbRepFn: Arbitrary[R => A],
    EqFA: Eq[F[A]],
    EqA: Eq[A]
  ): RuleSet =
    new DefaultRuleSet(
      name = "representable",
      parent = None,
      "index andThen tabulate = id" -> forAll(laws.indexTabulateIsId[A] _),
      "tabulate andThen index = id" -> forAll(laws.tabulateIndexIsId[A] _)
    )
}

object RepresentableTests {
  def apply[F[_], R](implicit RF: Representable.Aux[F, R]): RepresentableTests[F, R] =
    new RepresentableTests[F, R] {
      implicit override val laws: RepresentableLaws[F, R] = RepresentableLaws[F, R]
    }
}
