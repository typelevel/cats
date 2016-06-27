package cats.bench

import cats.data.Const
import cats.instances.string._
import cats.instances.list._
import cats.{Foldable, Traverse}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FoldBench {

  val chars: List[String] = ('a' to 'z').map(_.toString).toList

  /** Benchmark fold of Foldable[List] */
  @Benchmark
  def fold(): String =
    Foldable[List].fold(chars)

  /** Benchmark fold using traverse with Const */
  @Benchmark
  def traverseConst(): String =
    Traverse[List].traverse[Const[String, ?], String, String](chars)(Const(_)).getConst

}
