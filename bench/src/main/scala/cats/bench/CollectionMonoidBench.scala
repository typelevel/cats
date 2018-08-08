package cats.bench

import cats.Monoid
import cats.data.Catenable
import cats.implicits._
import chain.Chain
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class CollectionMonoidBench {

  private val largeList = (0 to 1000000).toList

  implicit def monoidChain[A]: Monoid[Chain[A]] = new Monoid[Chain[A]] {
    def empty: Chain[A] = Chain.empty[A]

    def combine(x: Chain[A], y: Chain[A]): Chain[A] = x ++ y
  }

  @Benchmark def accumulateCatenable: Catenable[Int] = largeList.foldMap(Catenable.singleton)
  @Benchmark def accumulateVector: Vector[Int] = largeList.foldMap(Vector(_))
  @Benchmark def accumulateList: List[Int] = largeList.foldMap(List(_))
  @Benchmark def accumulateChain: Chain[Int] = largeList.foldMap(Chain.single)
}
