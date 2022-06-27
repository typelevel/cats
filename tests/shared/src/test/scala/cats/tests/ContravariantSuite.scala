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

package cats.tests

import cats.data.Const
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.kernel.{Monoid, Semigroup}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{DecidableTests, MiniInt}
import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal}
import org.scalacheck.Prop._

class ContravariantSuite extends CatsSuite {

  test("narrow equals contramap(identity)") {
    implicit val constInst: Contravariant[Const[Int, *]] = Const.catsDataContravariantForConst[Int]
    forAll { (i: Int) =>
      val const: Const[Int, Option[Int]] = Const[Int, Option[Int]](i)
      val narrowed: Const[Int, Some[Int]] = constInst.narrow[Option[Int], Some[Int]](const)
      assert(narrowed === (constInst.contramap(const)(identity[Option[Int]](_: Some[Int]))))
      assert(narrowed eq const)
    }
  }

  checkAll(
    "Decidable[Predicate]",
    DecidableTests[Predicate].decidable[Boolean, Boolean, Boolean]
  )

  {
    implicit val predicateMonoid: Monoid[Predicate[MiniInt]] = ContravariantMonoidal.monoid[Predicate, MiniInt]
    checkAll("ContravariantMonoidal[Predicate].monoid", MonoidTests[Predicate[MiniInt]].monoid)
  }
  {
    implicit val predicateSemigroup: Semigroup[Predicate[MiniInt]] =
      ContravariantSemigroupal.semigroup[Predicate, MiniInt]
    checkAll("ContravariantSemigroupal[Predicate].semigroup", SemigroupTests[Predicate[MiniInt]].semigroup)
  }

}
