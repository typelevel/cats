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

import cats.data.HashMap
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.collection.immutable.{HashMap => SHashMap}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashMapBench {
  @Param(Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var hashMap: HashMap[Long, Int] = _
  var otherHashMap: HashMap[Long, Int] = _
  var scalaMap: SHashMap[Long, Int] = _
  var otherScalaMap: SHashMap[Long, Int] = _

  def hashMapOfSize(n: Int) = HashMap.fromSeq((1L to (n.toLong)).zipWithIndex)
  def scalaMapOfSize(n: Int) = SHashMap((1L to (n.toLong)).zipWithIndex: _*)

  @Setup(Level.Trial)
  def init(): Unit = {
    hashMap = hashMapOfSize(size)
    otherHashMap = hashMapOfSize(size)
    scalaMap = scalaMapOfSize(size)
    otherScalaMap = scalaMapOfSize(size)
  }

  @Benchmark
  def hashMapFromSeq(bh: Blackhole): Unit =
    bh.consume(hashMapOfSize(size))

  @Benchmark
  def scalaMapFromSeq(bh: Blackhole): Unit =
    bh.consume(scalaMapOfSize(size))

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashMapUpdated(bh: Blackhole): Unit = {
    var hs = hashMap
    var i = 0
    while (i < 1000) {
      hs = hs.updated(-i.toLong, i)
      i += 1
    }
    bh.consume(hs)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaMapUpdated(bh: Blackhole): Unit = {
    var ss = scalaMap
    var i = 0
    while (i < 1000) {
      ss = ss.updated(-i.toLong, i)
      i += 1
    }
    bh.consume(ss)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashMapRemoved(bh: Blackhole): Unit = {
    var hs = hashMap
    var i = 0L
    while (i < 1000L) {
      hs = hs.removed(i)
      i += 1L
    }
    bh.consume(hs)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaMapRemoved(bh: Blackhole): Unit = {
    var ss = scalaMap
    var i = 0L
    while (i < 1000L) {
      ss -= i
      i += 1L
    }
    bh.consume(ss)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashMapContains(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(hashMap.contains(i))
      i += 1L
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaMapContains(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(scalaMap.contains(i))
      i += 1L
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashMapGet(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(hashMap.get(i))
      i += 1L
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaMapGet(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(scalaMap.get(i))
      i += 1L
    }
  }

  @Benchmark
  def hashMapForeach(bh: Blackhole): Unit =
    hashMap.foreach((k, v) => bh.consume((k, v)))

  @Benchmark
  def scalaMapForeach(bh: Blackhole): Unit =
    scalaMap.foreach(bh.consume(_))

  @Benchmark
  def hashMapIterator(bh: Blackhole): Unit = {
    val it = hashMap.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def scalaMapIterator(bh: Blackhole): Unit = {
    val it = scalaMap.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def hashMapConcat(bh: Blackhole): Unit =
    bh.consume(hashMap.concat(otherHashMap))

  @Benchmark
  def scalaMapConcat(bh: Blackhole): Unit =
    bh.consume(scalaMap ++ otherScalaMap)

  @Benchmark
  def hashMapUniversalEquals(bh: Blackhole): Unit =
    bh.consume(hashMap == otherHashMap)

  @Benchmark
  def hashMapEqEquals(bh: Blackhole): Unit =
    bh.consume(hashMap === otherHashMap)

  @Benchmark
  def scalaMapUniversalEquals(bh: Blackhole): Unit =
    bh.consume(scalaMap == otherScalaMap)
}
