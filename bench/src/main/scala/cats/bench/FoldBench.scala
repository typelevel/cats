package cats.bench

import cats.data.Const
import cats.instances.string._
import cats.instances.list._
import cats.instances.vector._
import cats.instances.option._
import cats.{Foldable, Traverse}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FoldBench {

  val chars: List[String] = ('a' to 'z').map(_.toString).toList
  val charsVector: Vector[String] = chars.toVector
  val combineStringsSome: (String, String) => Option[String] = (s1, s2) => Some(s1 + s2)

  /** Benchmark fold of Foldable[List] */
  @Benchmark
  def fold(): String =
    Foldable[List].fold(chars)

  /** Benchmark fold using traverse with Const */
  @Benchmark
  def traverseConst(): String =
    Traverse[List].traverse[Const[String, *], String, String](chars)(Const(_)).getConst

  @Benchmark
  def vectorToListFoldM(): Option[String] =
    Foldable[List].foldM(charsVector.toList, "")(combineStringsSome)

  @Benchmark
  def vectorIndexFoldM(): Option[String] =
    Foldable[Vector].foldM(charsVector, "")(combineStringsSome)

}
