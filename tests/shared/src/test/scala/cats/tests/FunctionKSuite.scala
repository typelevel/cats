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

import cats.arrow.FunctionK
import cats.data.{EitherK, NonEmptyList}
import cats.syntax.eq.*
import cats.Id
import org.scalacheck.Prop.*

class FunctionKSuite extends CatsSuite {
  type OptionOfNel[+A] = Option[NonEmptyList[A]]

  val listToOption = new FunctionK[List, Option] { def apply[A](a: List[A]): Option[A] = a.headOption }
  val listToVector = new FunctionK[List, Vector] { def apply[A](a: List[A]): Vector[A] = a.toVector }
  val optionToList = new FunctionK[Option, List] { def apply[A](a: Option[A]): List[A] = a.toList }

  sealed trait Test1Algebra[A] {
    def v: A
  }

  case class Test1[A](v: A) extends Test1Algebra[A]

  sealed trait Test2Algebra[A] {
    def v: A
  }

  case class Test2[A](v: A) extends Test2Algebra[A]

  val Test1FK = new FunctionK[Test1Algebra, Id] { def apply[A](a: Test1Algebra[A]): A = a.v }
  val Test2FK = new FunctionK[Test2Algebra, Id] { def apply[A](a: Test2Algebra[A]): A = a.v }

  test("compose") {
    forAll { (list: List[Int]) =>
      val listToList = optionToList.compose(listToOption)
      assert(listToList(list) === list.take(1))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      assert(listToList(list) === list.take(1))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      assert(FunctionK.id[List].apply(list) === list)
    }
  }

  test("or") {
    val combinedInterpreter = Test1FK.or(Test2FK)
    forAll { (a: Int, b: Int) =>
      assert(combinedInterpreter(EitherK.left(Test1(a))) === a)
      assert(combinedInterpreter(EitherK.right(Test2(b))) === b)
    }
  }

  test("and") {
    val combinedInterpreter = listToOption.and(listToVector)
    forAll { (list: List[Int]) =>
      val prod = combinedInterpreter(list)
      assert(prod.first === list.headOption)
      assert(prod.second === list.toVector)
    }
  }
}
