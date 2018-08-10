package cats.bench

import cats.data.Catenable
import fs2.{Catenable => OldCat}
import chain.Chain
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class CatenableBench {

  private val smallCatenable = Catenable(1, 2, 3, 4, 5)
  private val smallFs2Catenable = OldCat(1, 2, 3, 4, 5)
  private val smallVector = Vector(1, 2, 3, 4, 5)
  private val smallList = List(1, 2, 3, 4, 5)
  private val smallChain = Chain(smallList)

  private val largeCatenable = (0 to 1000)
    .foldLeft(Catenable.empty[Int])((acc, _) => acc ++ Catenable.fromSeq(0 to 1000))
  private val largeFs2Catenable = OldCat.fromSeq(0 to 1000000)
  private val largeVector = (0 to 1000000).toVector
  private val largeList = (0 to 1000000).toList
  private val largeChain = (0 to 1000).foldLeft(Chain.empty[Int])((acc, _) => acc ++ Chain(0 to 1000))


  @Benchmark def mapSmallCatenable: Catenable[Int] = smallCatenable.map(_ + 1)
  @Benchmark def mapSmallFs2Catenable: OldCat[Int] = smallFs2Catenable.map(_ + 1)
  @Benchmark def mapSmallVector: Vector[Int] = smallVector.map(_ + 1)
  @Benchmark def mapSmallList: List[Int] = smallList.map(_ + 1)
  @Benchmark def mapSmallChain: Chain[Int] = smallChain.map(_ + 1)


  @Benchmark def mapLargeCatenable: Catenable[Int] = largeCatenable.map(_ + 1)
  @Benchmark def mapLargeFs2Catenable: OldCat[Int] = largeFs2Catenable.map(_ + 1)
  @Benchmark def mapLargeVector: Vector[Int] = largeVector.map(_ + 1)
  @Benchmark def mapLargeList: List[Int] = largeList.map(_ + 1)
  @Benchmark def mapLargeChain: Chain[Int] = largeChain.map(_ + 1)



  @Benchmark def foldLeftSmallCatenable: Int = smallCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallFs2Catenable: Int = smallFs2Catenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallVector: Int = smallVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallList: Int = smallList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallChain: Int = smallChain.foldLeft(0)(_ + _)


  @Benchmark def foldLeftLargeCatenable: Int = largeCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeFs2Catenable: Int = largeFs2Catenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeVector: Int = largeVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeList: Int = largeList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeChain: Int = largeChain.foldLeft(0)(_ + _)




  @Benchmark def consSmallCatenable: Catenable[Int] = 0 +: smallCatenable
  @Benchmark def consSmallFs2Catenable: OldCat[Int] = 0 +: smallFs2Catenable
  @Benchmark def consSmallVector: Vector[Int] = 0 +: smallVector
  @Benchmark def consSmallList: List[Int] = 0 +: smallList
  @Benchmark def consSmallChain: Chain[Int] = 0 +: smallChain

  @Benchmark def consLargeCatenable: Catenable[Int] = 0 +: largeCatenable
  @Benchmark def consLargeFs2Catenable: OldCat[Int] = 0 +: largeFs2Catenable
  @Benchmark def consLargeVector: Vector[Int] = 0 +: largeVector
  @Benchmark def consLargeList: List[Int] = 0 +: largeList
  @Benchmark def consLargeChain: Chain[Int] = 0 +: largeChain

  @Benchmark def createTinyCatenable: Catenable[Int] = Catenable(1)
  @Benchmark def createTinyFs2Catenable: OldCat[Int] = OldCat(1)
  @Benchmark def createTinyVector: Vector[Int] = Vector(1)
  @Benchmark def createTinyList: List[Int] = List(1)
  @Benchmark def createTinyChain: Chain[Int] = Chain.single(1)

  @Benchmark def createSmallCatenable: Catenable[Int] = Catenable(1, 2, 3, 4, 5)
  @Benchmark def createSmallFs2Catenable: OldCat[Int] = OldCat(1, 2, 3, 4, 5)
  @Benchmark def createSmallVector: Vector[Int] = Vector(1, 2, 3, 4, 5)
  @Benchmark def createSmallList: List[Int] = List(1, 2, 3, 4, 5)
  @Benchmark def createSmallChain: Chain[Int] = Chain(Seq(1, 2, 3, 4, 5))
}
