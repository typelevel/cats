package cats.bench

import cats.Applicative
import cats.instances.either._
import cats.instances.int._
import cats.instances.vector._
import cats.kernel.Monoid
import cats.syntax.foldable._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FoldMapABench {

  val xs: Vector[Int] = (1 to 10000).toVector
  val f: Int => Either[Int, Int] = x => if (x >= 5000) Left(x) else Right(x)
  val g: Int => Either[Int, Int] = x => if (x >= 5000) Right(0) else Right(x)
  val M: Monoid[Either[Int, Int]] = Applicative.monoid[Either[Int, ?], Int]

  @Benchmark
  def foldMapMShortCircuit: Either[Int, Int] = xs.foldMapM(f)

  @Benchmark
  def foldMapAShortCircuit: Either[Int, Int] = xs.foldMapA(f)

  // Note that this doesn't actually short-circuit; it will keep combining
  // even after it reaches the `Left` value.
  @Benchmark
  def foldLeftShortCircuit: Either[Int, Int] = xs.foldLeft(M.empty) {
    case (acc, x) => M.combine(acc, f(x))
  }

  @Benchmark
  def foldMapM: Either[Int, Int] = xs.foldMapM(g)

  @Benchmark
  def foldMapA: Either[Int, Int] = xs.foldMapA(g)

  @Benchmark
  def foldLeft: Either[Int, Int] = xs.foldLeft(M.empty) {
    case (acc, x) => M.combine(acc, g(x))
  }
}
