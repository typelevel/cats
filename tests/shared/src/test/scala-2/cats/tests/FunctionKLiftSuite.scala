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

import cats.Applicative
import cats.data.NonEmptyList
import cats.arrow.FunctionK
import cats.syntax.all.*
import org.scalacheck.Prop.*
import cats.laws.discipline.arbitrary.*

class FunctionKLiftSuite extends CatsSuite {
  type OptionOfNel[+A] = Option[NonEmptyList[A]]

  test("lift simple unary") {
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === (optionToList(a)))
    }

    val fO2I: FunctionK[Option, Iterable] = FunctionK.lift(Option.option2Iterable _)
    forAll { (a: Option[String]) =>
      assert(fO2I(a).toList === (Option.option2Iterable(a).toList))
    }

    val fNelFromListUnsafe = FunctionK.lift(NonEmptyList.fromListUnsafe _)
    forAll { (a: NonEmptyList[Int]) =>
      assert(fNelFromListUnsafe(a.toList) === (NonEmptyList.fromListUnsafe(a.toList)))
    }
  }

  test("hygiene") {
    trait FunctionK
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = cats.arrow.FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === (optionToList(a)))
    }
  }

  test("lift compound unary") {
    val fNelFromList = FunctionK.lift[List, λ[α => Option[NonEmptyList[α]]]](NonEmptyList.fromList _)
    forAll { (a: List[String]) =>
      assert(fNelFromList(a) === (NonEmptyList.fromList(a)))
    }
  }

  test("lift eta-expanded function") {
    val fSomeNel = FunctionK.lift[NonEmptyList, OptionOfNel](Applicative[Option].pure)
    forAll { (a: NonEmptyList[Int]) =>
      assert(fSomeNel(a) === Some(a))
    }
  }

  { // lifting concrete types should fail to compile
    def sample[A](option: Option[A]): List[A] = option.toList
    assert(compileErrors("FunctionK.lift(sample[String])").nonEmpty)
    assert(compileErrors("FunctionK.lift(sample[Nothing])").nonEmpty)
  }
}
