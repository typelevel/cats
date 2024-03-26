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

import cats.{Eval, Traverse, TraverseFilter}
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import org.openjdk.jmh.infra.Blackhole
import cats.data.Chain

@State(Scope.Benchmark)
class TraverseBench {
  val listT: Traverse[List] = Traverse[List]
  val listTFilter: TraverseFilter[List] = TraverseFilter[List]
  val chainTFilter: TraverseFilter[Chain] = TraverseFilter[Chain]

  val vectorT: Traverse[Vector] = Traverse[Vector]
  val vectorTFilter: TraverseFilter[Vector] = TraverseFilter[Vector]

  val chainT: Traverse[Chain] = Traverse[Chain]

  // the unit of CPU work per iteration
  private[this] val Work: Long = 10

  private[this] case object Failure extends RuntimeException

  @Param(Array("10000"))
  var length: Int = _

  var list: List[Int] = _
  var vector: Vector[Int] = _
  var chain: Chain[Int] = _

  @Setup
  def setup(): Unit = {
    list = 0.until(length).toList
    vector = 0.until(length).toVector
    chain = Chain.fromSeq(0.until(length))
  }

  @Benchmark
  def traverseList(bh: Blackhole) = {
    val result = listT.traverse(list) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverseListError(bh: Blackhole) = {
    val result = listT.traverse(list) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)

        if (i == length * 0.3) {
          throw Failure
        }

        i * 2
      }
    }

    try {
      bh.consume(result.value)
    } catch {
      case Failure => ()
    }
  }

  @Benchmark
  def traverse_List(bh: Blackhole) = {
    val result = listT.traverse_(list) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverseFilterList(bh: Blackhole) = {
    val result = listTFilter.traverseFilter(list) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        if (i % 2 == 0) Some(i * 2) else None
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def mapList(bh: Blackhole) = {
    val results = list.map { i =>
      val inner = Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }

      // we just want to force the allocation to level the playing field
      inner.value
    }

    bh.consume(results)
  }

  @Benchmark
  def filterList(bh: Blackhole) = {
    val results = list.flatMap { i =>
      val inner = Eval.later {
        Blackhole.consumeCPU(Work)
        if (i % 2 == 0) Some(i * 2) else None
      }

      // we just want to force the allocation to level the playing field
      inner.value
    }

    bh.consume(results)
  }

  @Benchmark
  def traverseVector(bh: Blackhole) = {
    val result = vectorT.traverse(vector) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverse_Vector(bh: Blackhole) = {
    val result = vectorT.traverse_(vector) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverseVectorError(bh: Blackhole) = {
    val result = vectorT.traverse(vector) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)

        if (i == length * 0.3) {
          throw Failure
        }

        i * 2
      }
    }

    try {
      bh.consume(result.value)
    } catch {
      case Failure => ()
    }
  }

  @Benchmark
  def traverseFilterVector(bh: Blackhole) = {
    val result = vectorTFilter.traverseFilter(vector) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        if (i % 2 == 0) Some(i * 2) else None
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def mapVector(bh: Blackhole) = {
    val results = vector.map { i =>
      val inner = Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }

      // we just want to force the allocation to level the playing field
      inner.value
    }

    bh.consume(results)
  }

  @Benchmark
  def filterVector(bh: Blackhole) = {
    val results = vector.flatMap { i =>
      val inner = Eval.later {
        Blackhole.consumeCPU(Work)
        if (i % 2 == 0) Some(i * 2) else None
      }

      // we just want to force the allocation to level the playing field
      inner.value
    }

    bh.consume(results)
  }

  @Benchmark
  def traverseChain(bh: Blackhole) = {
    val result = chainT.traverse(chain) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverse_Chain(bh: Blackhole) = {
    val result = chainT.traverse_(chain) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        i * 2
      }
    }

    bh.consume(result.value)
  }

  @Benchmark
  def traverseChainError(bh: Blackhole) = {
    val result = chainT.traverse(chain) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)

        if (i == length * 0.3) {
          throw Failure
        }

        i * 2
      }
    }

    try {
      bh.consume(result.value)
    } catch {
      case Failure => ()
    }
  }

  @Benchmark
  def traverseFilterChain(bh: Blackhole) = {
    val result = chainTFilter.traverseFilter(chain) { i =>
      Eval.later {
        Blackhole.consumeCPU(Work)
        if (i % 2 == 0) Some(i * 2) else None
      }
    }

    bh.consume(result.value)
  }
}
