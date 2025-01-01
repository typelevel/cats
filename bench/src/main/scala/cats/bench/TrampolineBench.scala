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

package cats.bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import cats.*
import cats.syntax.all.*
import cats.free.Trampoline
import scala.util.control.TailCalls

@State(Scope.Benchmark)
class TrampolineBench {

  val N = 15

  @Benchmark
  def eval(): Int = evalFib(N).value

  def evalFib(n: Int): Eval[Int] =
    if (n < 2) Eval.now(n)
    else
      for {
        x <- Eval.defer(evalFib(n - 1))
        y <- Eval.defer(evalFib(n - 2))
      } yield x + y
  @Benchmark
  def trampoline(): Int = trampolineFib(N).run

  def trampolineFib(n: Int): Trampoline[Int] =
    if (n < 2) Trampoline.done(n)
    else
      for {
        x <- Trampoline.defer(trampolineFib(n - 1))
        y <- Trampoline.defer(trampolineFib(n - 2))
      } yield x + y

  @Benchmark
  def stdlib(): Int = stdlibFib(N).result

  def stdlibFib(n: Int): TailCalls.TailRec[Int] =
    if (n < 2) TailCalls.done(n)
    else
      for {
        x <- TailCalls.tailcall(stdlibFib(n - 1))
        y <- TailCalls.tailcall(stdlibFib(n - 2))
      } yield x + y
}
