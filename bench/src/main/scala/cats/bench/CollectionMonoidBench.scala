package cats.bench

import cats.Monoid
import cats.data.Chain
import cats.implicits._
import chain.{Chain => OldChain}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class CollectionMonoidBench {

  private val largeList = (0 to 1000000).toList

  implicit def monoidOldChain[A]: Monoid[OldChain[A]] = new Monoid[OldChain[A]] {
    def empty: OldChain[A] = OldChain.empty[A]

    def combine(x: OldChain[A], y: OldChain[A]): OldChain[A] = x ++ y
  }

  @Benchmark def accumulateChain: Chain[Int] = largeList.foldMap(Chain.one)
  @Benchmark def accumulateVector: Vector[Int] = largeList.foldMap(Vector(_))
  @Benchmark def accumulateList: List[Int] = largeList.foldMap(List(_))
  @Benchmark def accumulateOldChain: OldChain[Int] = largeList.foldMap(OldChain.single)
}
