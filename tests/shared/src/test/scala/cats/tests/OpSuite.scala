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

import cats.arrow.*
import cats.data.{Kleisli, Op}
import cats.kernel.Eq
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.kernel.laws.discipline.EqTests

class OpSuite extends CatsSuite {
  {
    implicit val catsDataEqForOp: Eq[Op[Function1, Int, MiniInt]] = Op.catsDataEqForOp[Function1, Int, MiniInt]
    checkAll("Op[Function1, Int, MiniInt]", EqTests[Op[Function1, Int, MiniInt]].eqv)
    checkAll("Eq[Op[Function1, Int, MiniInt]]", SerializableTests.serializable(Eq[Op[Function1, Int, MiniInt]]))
  }

  {
    implicit val catsDataCategoryForOp: Category[Op[Function1, *, *]] = Op.catsDataCategoryForOp[Function1]
    checkAll("Op[Function1, *, *]", CategoryTests[Op[Function1, *, *]].category[Char, MiniInt, Char, Boolean])
    checkAll("Category[Op[Function1, *, *]]", SerializableTests.serializable(Category[Op[Function1, *, *]]))
  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    // Arr is Function1
    Category[Op[Function1, *, *]]
    Compose[Op[Function1, *, *]]
    Eq[Op[Function1, Char, MiniInt]]

    // Arr is Kleisli[Option, *, *]
    Category[Op[Kleisli[Option, *, *], *, *]]
    Compose[Op[Kleisli[Option, *, *], *, *]]
  }
}
