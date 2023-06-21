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

import cats.Comonad
import cats.data.{RepresentableStore, Store}
import cats.kernel.Eq
import cats.laws.discipline.{ComonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen}
import cats.syntax.eq._
import org.scalacheck.Prop._

class RepresentableStoreSuite extends CatsSuite {

  // Note: The Eq instance for Function1 causes this to run excessively long, and timeout the travis build
  // checkAll("Comonad[Store[String, *]]", ComonadTests[Store[String, *]].comonad[Int, Int, Int])

  {
    implicit val pairComonad: Comonad[RepresentableStore[λ[P => (P, P)], Boolean, *]] =
      RepresentableStore.catsDataRepresentableStoreComonad[λ[P => (P, P)], Boolean]
    implicit val eqStore: Eq[RepresentableStore[λ[P => (P, P)], Boolean, Int]] =
      cats.laws.discipline.eq.catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, Int]
    implicit val eqStoreStore
      : Eq[RepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[λ[P => (P, P)], Boolean, Int]]] =
      cats.laws.discipline.eq
        .catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[λ[P => (P, P)], Boolean, Int]]
    implicit val eqStoreStoreStore: Eq[RepresentableStore[
      λ[P => (P, P)],
      Boolean,
      RepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[λ[P => (P, P)], Boolean, Int]]
    ]] =
      cats.laws.discipline.eq.catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[
        λ[P => (P, P)],
        Boolean,
        RepresentableStore[λ[P => (P, P)], Boolean, Int]
      ]]
    checkAll("Comonad[RepresentableStore[λ[P => (P, P)], Boolean, *]]",
             ComonadTests[RepresentableStore[λ[P => (P, P)], Boolean, *]].comonad[Int, Int, Int]
    )

    checkAll("Comonad[RepresentableStore[λ[P => (P, P)], Boolean, *]]",
             SerializableTests.serializable(Comonad[RepresentableStore[λ[P => (P, P)], Boolean, *]])
    )
  }

  test("extract and peek are consistent") {
    forAll { (store: Store[String, String]) =>
      assert(store.extract === (store.peek(store.index)))
    }
  }

  test("use store alias constructor") {
    forAll { (f: String => Int, s: String) =>
      val store = Store(f, s)
      assert(store.extract === (f(s)))
    }
  }
}
