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

import cats.{Align, Alternative, SemigroupK}
import cats.data.{Chain, Validated}
import cats.laws.discipline.AlignTests
import cats.laws.discipline.arbitrary.*

class SemigroupKSuite extends CatsSuite {
  implicit val listwrapperSemigroupK: Alternative[ListWrapper] = ListWrapper.alternative
  implicit val listwrapperAlign: Align[ListWrapper] = SemigroupK.align[ListWrapper]
  checkAll("SemigroupK[ListWrapper].align", AlignTests[ListWrapper].align[Int, Int, Int, Int])

  implicit val validatedAlign: Align[Validated[String, *]] = SemigroupK.align[Validated[String, *]]
  checkAll("SemigroupK[Validated].align", AlignTests[Validated[String, *]].align[Int, Int, Int, Int])

  implicit val chainAlign: Align[Chain] = SemigroupK.align[Chain]
  checkAll("SemigroupK[Chain].align", AlignTests[Chain].align[Int, Int, Int, Int])
}
