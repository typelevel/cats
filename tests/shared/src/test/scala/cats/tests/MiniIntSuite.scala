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

import cats.kernel.{BoundedSemilattice, CommutativeGroup, CommutativeMonoid, Hash, Order}
import cats.kernel.laws.discipline.*
import cats.laws.discipline.MiniInt
import cats.laws.discipline.MiniInt.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.Gen
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class MiniIntSuite extends CatsSuite {
  checkAll("MiniInt", OrderTests[MiniInt].order)
  checkAll("Order[MiniInt]", SerializableTests.serializable(Order[MiniInt]))

  checkAll("MiniInt", HashTests[MiniInt].hash)
  checkAll("Hash[MiniInt]", SerializableTests.serializable(Hash[MiniInt]))

  {
    implicit val g: CommutativeGroup[MiniInt] = miniIntAddition
    checkAll("MiniInt addition", CommutativeGroupTests[MiniInt].commutativeGroup)
    checkAll("CommutativeGroup[MiniInt] addition", SerializableTests.serializable(miniIntAddition))
  }

  {
    implicit val m: CommutativeMonoid[MiniInt] = miniIntMultiplication
    checkAll("MiniInt addition", CommutativeMonoidTests[MiniInt].commutativeMonoid)
    checkAll("CommutativeMonoid[MiniInt] multiplication", SerializableTests.serializable(miniIntMultiplication))
  }

  {
    implicit val b: BoundedSemilattice[MiniInt] = miniIntOr
    checkAll("MiniInt |", BoundedSemilatticeTests[MiniInt].boundedSemilattice)
    checkAll("BoundedSemilattice[MiniInt] |", SerializableTests.serializable(miniIntOr))
  }

  test("int roundtrip") {
    forAll { (i: MiniInt) =>
      assert(MiniInt.fromInt(i.toInt) === (Some(i)))
    }
  }

  test("int bounds") {
    forAll(Gen.chooseNum(MiniInt.minIntValue, MiniInt.maxIntValue)) { (i: Int) =>
      assert(MiniInt.fromInt(i).map(_.toInt) === (Some(i)))
    }
  }
}
