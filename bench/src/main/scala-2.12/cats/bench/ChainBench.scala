package cats.bench

import cats.data.Chain
import fs2.Catenable
import chain.{Chain => OldChain}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class ChainBench {

  private val intOption = Option(1)
  private val smallChain = Chain(1, 2, 3, 4, 5)
  private val smallCatenable = Catenable(1, 2, 3, 4, 5)
  private val smallVector = Vector(1, 2, 3, 4, 5)
  private val smallList = List(1, 2, 3, 4, 5)
  private val smallOldChain = OldChain(smallList)

  private val largeChain = (0 to 1000)
    .foldLeft(Chain.empty[Int])((acc, _) => acc ++ Chain.fromSeq(0 to 1000))
  private val largeCatenable = Catenable.fromSeq(0 to 1000000)
  private val largeVector = (0 to 1000000).toVector
  private val largeList = (0 to 1000000).toList
  private val largeOldChain = (0 to 1000).foldLeft(OldChain.empty[Int])((acc, _) => acc ++ OldChain(0 to 1000))
  @Benchmark def mapSmallChain: Chain[Int] = smallChain.map(_ + 1)
  @Benchmark def mapSmallCatenable: Catenable[Int] = smallCatenable.map(_ + 1)
  @Benchmark def mapSmallVector: Vector[Int] = smallVector.map(_ + 1)
  @Benchmark def mapSmallList: List[Int] = smallList.map(_ + 1)
  @Benchmark def mapSmallOldChain: OldChain[Int] = smallOldChain.map(_ + 1)
  @Benchmark def mapLargeChain: Chain[Int] = largeChain.map(_ + 1)
  @Benchmark def mapLargeCatenable: Catenable[Int] = largeCatenable.map(_ + 1)
  @Benchmark def mapLargeVector: Vector[Int] = largeVector.map(_ + 1)
  @Benchmark def mapLargeList: List[Int] = largeList.map(_ + 1)
  @Benchmark def mapLargeOldChain: OldChain[Int] = largeOldChain.map(_ + 1)
  @Benchmark def foldLeftSmallChain: Int = smallChain.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallCatenable: Int = smallCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallVector: Int = smallVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallList: Int = smallList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallOldChain: Int = smallOldChain.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeChain: Int = largeChain.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeCatenable: Int = largeCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeVector: Int = largeVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeList: Int = largeList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeOldChain: Int = largeOldChain.foldLeft(0)(_ + _)
  @Benchmark def consSmallChain: Chain[Int] = 0 +: smallChain
  @Benchmark def consSmallCatenable: Catenable[Int] = 0 +: smallCatenable
  @Benchmark def consSmallVector: Vector[Int] = 0 +: smallVector
  @Benchmark def consSmallList: List[Int] = 0 +: smallList
  @Benchmark def consSmallOldChain: OldChain[Int] = 0 +: smallOldChain

  @Benchmark def consLargeChain: Chain[Int] = 0 +: largeChain
  @Benchmark def consLargeCatenable: Catenable[Int] = 0 +: largeCatenable
  @Benchmark def consLargeVector: Vector[Int] = 0 +: largeVector
  @Benchmark def consLargeList: List[Int] = 0 +: largeList
  @Benchmark def consLargeOldChain: OldChain[Int] = 0 +: largeOldChain

  @Benchmark def createTinyChain: Chain[Int] = Chain(1)
  @Benchmark def createTinyCatenable: Catenable[Int] = Catenable(1)
  @Benchmark def createTinyVector: Vector[Int] = Vector(1)
  @Benchmark def createTinyList: List[Int] = List(1)
  @Benchmark def createTinyOldChain: OldChain[Int] = OldChain.single(1)

  @Benchmark def createSmallChain: Chain[Int] = Chain(1, 2, 3, 4, 5)
  @Benchmark def createSmallCatenable: Catenable[Int] = Catenable(1, 2, 3, 4, 5)
  @Benchmark def createSmallVector: Vector[Int] = Vector(1, 2, 3, 4, 5)
  @Benchmark def createSmallList: List[Int] = List(1, 2, 3, 4, 5)
  @Benchmark def createSmallOldChain: OldChain[Int] = OldChain(Seq(1, 2, 3, 4, 5))

  @Benchmark def createChainSeqOption: Chain[Int] = Chain.fromSeq(intOption.toSeq)
  @Benchmark def createChainOption: Chain[Int] = Chain.fromOption(intOption)
}
