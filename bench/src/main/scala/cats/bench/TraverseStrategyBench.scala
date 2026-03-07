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

import cats.{Eval, Traverse}
import cats.instances.list.*
import cats.instances.vector.*
import cats.instances.either.*
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Benchmark)
class TraverseStrategyBench {
  val listT: Traverse[List] = Traverse[List]
  val vectorT: Traverse[Vector] = Traverse[Vector]

  @Param(Array("100", "1000", "10000"))
  var length: Int = _

  var list: List[Int] = _
  var vector: Vector[Int] = _
  var evalVector: Vector[Eval[Int]] = _

  @Setup
  def setup(): Unit = {
    list = (0 until length).toList
    vector = (0 until length).toVector
    evalVector = (0 until length).toVector.map(Eval.now)
  }

  @Benchmark
  def traverseEvalList(bh: Blackhole): Unit = {
    val result = listT.traverse(list) { i => Eval.now(i) }
    bh.consume(result.value)
  }

  @Benchmark
  def traverseVoidEvalList(bh: Blackhole): Unit = {
    val result = listT.traverseVoid(list) { i => Eval.now(i) }
    bh.consume(result.value)
  }

  @Benchmark
  def traverseEitherList(bh: Blackhole): Unit = {
    val result = listT.traverse(list) { i =>
      if (i > length + 100) Left("Error") else Right(i)
    }
    bh.consume(result match {
      case Right(l) => l.headOption
      case Left(_)  => None
    })
  }

  @Benchmark
  def traverseVoidEitherList(bh: Blackhole): Unit = {
    val result = listT.traverseVoid(list) { i =>
      if (i > length + 100) Left("Error") else Right(i)
    }
    bh.consume(result match {
      case Right(_) => true
      case Left(_)  => false
    })
  }

  @Benchmark
  def sequenceEvalVector(bh: Blackhole): Unit = {
    val result = vectorT.sequence(evalVector)
    bh.consume(result.value)
  }
}
