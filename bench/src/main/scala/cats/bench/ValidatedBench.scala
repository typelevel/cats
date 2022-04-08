/*
 * Copyright (c) 2022 Typelevel
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

package cats.bench

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class ValidatedBench {
  val x: Validated[String, Int] = Validated.valid(100)

  def bimapPointfree[E, A, EE, AA](v: Validated[E, A])(fe: E => EE, fa: A => AA): Validated[EE, AA] =
    v.fold(fe.andThen(Invalid.apply), fa.andThen(Valid.apply))

  def bimapPointfull[E, A, EE, AA](v: Validated[E, A])(fe: E => EE, fa: A => AA): Validated[EE, AA] =
    v match {
      case Valid(a)   => Valid(fa(a))
      case Invalid(e) => Invalid(fe(e))
    }

  @Benchmark
  def pointfull: Validated[Int, Int] = bimapPointfull(x)(_.length, _ + 1)
  @Benchmark
  def pointfree: Validated[Int, Int] = bimapPointfree(x)(_.length, _ + 1)
}
