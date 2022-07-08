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

import cats.data.Chain
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class ChainBench {

  private val intOption = Option(1)
  private val smallChain = Chain(1, 2, 3, 4, 5)
  private val smallVector = Vector(1, 2, 3, 4, 5)
  private val smallList = List(1, 2, 3, 4, 5)

  private val largeChain = (0 to 1000)
    .foldLeft(Chain.empty[Int])((acc, _) => acc ++ Chain.fromSeq(0 to 1000))
  private val largeVector = (0 to 1000000).toVector
  private val largeList = (0 to 1000000).toList
  @Benchmark def mapSmallChain: Chain[Int] = smallChain.map(_ + 1)
  @Benchmark def mapSmallVector: Vector[Int] = smallVector.map(_ + 1)
  @Benchmark def mapSmallList: List[Int] = smallList.map(_ + 1)
  @Benchmark def mapLargeChain: Chain[Int] = largeChain.map(_ + 1)
  @Benchmark def mapLargeVector: Vector[Int] = largeVector.map(_ + 1)
  @Benchmark def mapLargeList: List[Int] = largeList.map(_ + 1)
  @Benchmark def foldLeftSmallChain: Int = smallChain.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallVector: Int = smallVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallList: Int = smallList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeChain: Int = largeChain.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeVector: Int = largeVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeList: Int = largeList.foldLeft(0)(_ + _)
  @Benchmark def consSmallChain: Chain[Int] = 0 +: smallChain
  @Benchmark def consSmallVector: Vector[Int] = 0 +: smallVector
  @Benchmark def consSmallList: List[Int] = 0 +: smallList

  @Benchmark def consLargeChain: Chain[Int] = 0 +: largeChain
  @Benchmark def consLargeVector: Vector[Int] = 0 +: largeVector
  @Benchmark def consLargeList: List[Int] = 0 +: largeList

  @Benchmark def createTinyChain: Chain[Int] = Chain(1)
  @Benchmark def createTinyVector: Vector[Int] = Vector(1)
  @Benchmark def createTinyList: List[Int] = List(1)

  @Benchmark def createSmallChain: Chain[Int] = Chain(1, 2, 3, 4, 5)
  @Benchmark def createSmallVector: Vector[Int] = Vector(1, 2, 3, 4, 5)
  @Benchmark def createSmallList: List[Int] = List(1, 2, 3, 4, 5)

  @Benchmark def createChainSeqOption: Chain[Int] = Chain.fromSeq(intOption.toSeq)
  @Benchmark def createChainOption: Chain[Int] = Chain.fromOption(intOption)

  @Benchmark def reverseLargeList: List[Int] = largeList.reverse
  @Benchmark def reverseLargeChain: Chain[Int] = largeChain.reverse

  @Benchmark def lengthLargeList: Int = largeList.length
  @Benchmark def lengthLargeChain: Long = largeChain.length
}
