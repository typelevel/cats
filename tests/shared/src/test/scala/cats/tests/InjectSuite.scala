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

import cats.Inject
import cats.laws.discipline.InjectTests
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class InjectSuite extends CatsSuite {

  type StringOrInt = Either[String, Int]

  test("inj & prj") {
    def distr[F](f1: F, f2: F)(implicit I0: Inject[String, F], I1: Inject[Int, F]): Option[String] =
      for {
        x <- I0.prj(f1)
        y <- I1.prj(f2)
      } yield s"$x $y"

    forAll { (x: String, y: Int) =>
      val expr1: StringOrInt = Inject[String, StringOrInt].inj(x)
      val expr2: StringOrInt = Inject[Int, StringOrInt].inj(y)
      val res = distr(expr1, expr2)
      assert(res === (Some(s"$x $y")))
    }
  }

  test("apply & unapply") {
    def distr[F](f1: F, f2: F)(implicit I0: Inject[String, F], I1: Inject[Int, F]): Option[String] =
      for {
        x <- I0.unapply(f1)
        y <- I1.unapply(f2)
      } yield s"$x $y"

    forAll { (x: String, y: Int) =>
      val expr1: StringOrInt = Inject[String, StringOrInt].apply(x)
      val expr2: StringOrInt = Inject[Int, StringOrInt].apply(y)
      val res = distr(expr1, expr2)
      assert(res === (Some(s"$x $y")))
    }
  }

  test("apply in left") {
    forAll { (y: String) =>
      assert(Inject[String, StringOrInt].inj(y) == Left(y) === true)
    }
  }

  test("apply in right") {
    forAll { (y: Int) =>
      assert(Inject[Int, StringOrInt].inj(y) == Right(y) === true)
    }
  }

  test("null identity") {
    val stringNull = null.asInstanceOf[String]
    assert(Inject.catsReflexiveInjectInstance[String].inj(stringNull) === stringNull)
    assert(Inject.catsReflexiveInjectInstance[String].prj(stringNull) === (Some(stringNull)))
  }

  checkAll("Inject[String, StringOrInt]", InjectTests[String, StringOrInt].inject)
  checkAll("Inject[Int, StringOrInt]", InjectTests[Int, StringOrInt].inject)
}
