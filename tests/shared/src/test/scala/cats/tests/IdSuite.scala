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

import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.{Bimonad, CommutativeMonad, Id, Reducible, Semigroupal, Traverse}
import org.scalacheck.Prop.*
import cats.Align

class IdSuite extends CatsSuite {
  implicit val iso: SemigroupalTests.Isomorphisms[Id] =
    SemigroupalTests.Isomorphisms.invariant[Id]

  checkAll("Id[Int]", BimonadTests[Id].bimonad[Int, Int, Int])
  checkAll("Bimonad[Id]", SerializableTests.serializable(Bimonad[Id]))

  checkAll("Id[Int]", CommutativeMonadTests[Id].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Id]", SerializableTests.serializable(CommutativeMonad[Id]))

  checkAll("Id[Int]", TraverseTests[Id].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Id]", SerializableTests.serializable(Traverse[Id]))

  checkAll("Id[Int]", ReducibleTests[Id].reducible[Option, Int, Int])
  checkAll("Reducible[Id]", SerializableTests.serializable(Reducible[Id]))

  checkAll("Id[Int]", AlignTests[Id].align[Int, Int, Int, Int])
  checkAll("Align[Id]", SerializableTests.serializable(Align[Id]))

  test("Id#apply") {
    forAll { (i: Int) =>
      val id = Id(i)
      assert(id === (i: Id[Int]))
      assert(id === i.pure[Id])
      assert(id === i)
    }
  }

  def summonInstances(): Unit = {
    Semigroupal[Id]
    ()
  }
}
