package cats.bench

import cats.Eval
import cats.data.StateT
import org.openjdk.jmh.annotations._

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
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
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
