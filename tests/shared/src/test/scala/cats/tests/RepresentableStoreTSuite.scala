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

import cats.*
import cats.data.{StoreT, Validated}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import cats.data.RepresentableStoreT
import org.scalacheck.{Arbitrary, Cogen}

class RepresentableStoreTSuite extends CatsSuite {

  implicit val monoid: Monoid[MiniInt] = MiniInt.miniIntAddition

  implicit val scala2_12_makes_me_sad: Comonad[StoreT[Id, MiniInt, *]] =
    RepresentableStoreT.comonadForStoreT[Id, Function1[MiniInt, *], MiniInt]
  // Like, really, really, really sad
  val a: Arbitrary[Int] = implicitly[Arbitrary[Int]]
  val b: Eq[Int] = Eq[Int]
  val c: Arbitrary[StoreT[Id, MiniInt, Int]] = implicitly[Arbitrary[StoreT[Id, MiniInt, Int]]]
  val d: Cogen[Int] = implicitly[Cogen[Int]]
  val e: Cogen[StoreT[Id, MiniInt, Int]] = implicitly[Cogen[StoreT[Id, MiniInt, Int]]]
  val f: Eq[StoreT[Id, MiniInt, Int]] = Eq[StoreT[Id, MiniInt, Int]]
  val g: Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]] = Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]
  val h: Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]] =
    Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]]

  checkAll(
    "StoreT[Id, MiniInt, *]",
    ComonadTests[StoreT[Id, MiniInt, *]].comonad[Int, Int, Int](using a, b, a, b, a, b, c, d, d, d, e, e, f, g, h, f, f)
  )

  checkAll("Comonad[StoreT[Id, MiniInt, *]]", SerializableTests.serializable(Comonad[StoreT[Id, MiniInt, *]]))

  checkAll("StoreT[Validated[String, *], MiniInt, *]]",
           ApplicativeTests[StoreT[Validated[String, *], MiniInt, *]].applicative[MiniInt, MiniInt, MiniInt]
  )

  checkAll("Comonad[StoreT[Validated[String, *], MiniInt, *]]",
           SerializableTests.serializable(Applicative[StoreT[Validated[String, *], MiniInt, *]])
  )

  test("extract and peek are consistent") {
    forAll { (store: StoreT[Id, String, String]) =>
      assert(store.extract === (store.peek(store.index)))
    }
  }

}
