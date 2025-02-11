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
import cats.arrow.{ArrowChoice, CommutativeArrow}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.{ArrowChoiceTests, CommutativeArrowTests, MiniInt}

class PartialFunctionSuite extends CatsSuite {

  checkAll("ArrowChoice[PartialFunction]", SerializableTests.serializable(ArrowChoice[PartialFunction]))

  checkAll("PartialFunction",
           ArrowChoiceTests[PartialFunction].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )

  checkAll("CommutativeArrow[PartialFunction]", SerializableTests.serializable(CommutativeArrow[PartialFunction]))
  checkAll(
    "PartialFunction",
    CommutativeArrowTests[PartialFunction].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )

}
