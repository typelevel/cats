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

import cats.Eval
import cats.data.StateT
import org.openjdk.jmh.annotations.*

/**
 * To run:
 *
 *     bench/jmh:run -i 10 -wi 10 -f 2 -t 1 cats.bench.StateTBench
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
class StateTBench {
  @Param(Array("10"))
  var count: Int = _

  @Benchmark
  def single(): Long =
    randLong.run(32311).value._2

  @Benchmark
  def repeatedLeftBinds(): Int = {
    var state = randInt
    var i = 0
    while (i < count) {
      state = state.flatMap(int => randInt.map(_ + int))
      i += 1
    }
    state.run(32312).value._2
  }

  @Benchmark
  def repeatedRightBinds(): Int = {
    var state = randInt
    var i = 0
    while (i < count) {
      val oldS = state
      state = randInt.flatMap(int => oldS.map(_ + int))
      i += 1
    }
    state.run(32313).value._2
  }

  def fn(seed: Long): Eval[(Long, Int)] =
    Eval.now {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val n = (newSeed >>> 16).toInt
      (newSeed, n)
    }

  val randInt: StateT[Eval, Long, Int] =
    StateT(fn)

  val randLong: StateT[Eval, Long, Long] =
    for {
      int1 <- randInt
      int2 <- randInt
    } yield {
      (int1.toLong << 32) | int2
    }
}
