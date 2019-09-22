package cats.bench

import cats.Eval
import cats.data.Kleisli
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class KleisliBench {

  @Benchmark
  def baselineFlatMap(): Long = {
    def pure(x: Long) = Eval.now(x)
    val count = 100
    val result = (0 until count).foldLeft(pure(0)) { (acc, x) =>
      acc.flatMap(y => pure(x+y))
    }
    result.value
  }

  @Benchmark
  def flatMap(): Long = {
    def pure(x: Long) = Kleisli.pure[Eval, Unit, Long](x)
    val count = 100
    val result = (0 until count).foldLeft(pure(0)) { (acc, x) =>
      acc.flatMap(y => pure(x+y))
    }
    result.run(()).value
  }

}
