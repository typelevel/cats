package cats.bench

import cats.data.Catenable
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class CatenableBench {

  private val smallCatenable = Catenable(1, 2, 3, 4, 5)
  private val smallVector = Vector(1, 2, 3, 4, 5)
  private val smallList = List(1, 2, 3, 4, 5)

  private val largeCatenable = Catenable.fromSeq(0 to 1000000)
  private val largeVector = (0 to 1000000).toVector
  private val largeList = (0 to 1000000).toList

  private val largeNestedCatenable = largeList.map(Catenable.singleton)
  private val largeNestedVector = largeList.map(Vector(_))
  private val largeNestedList = largeList.map(List(_))

  @Benchmark def mapSmallCatenable: Catenable[Int] = smallCatenable.map(_ + 1)
  @Benchmark def mapSmallVector: Vector[Int] = smallVector.map(_ + 1)
  @Benchmark def mapSmallList: List[Int] = smallList.map(_ + 1)
  @Benchmark def mapLargeCatenable: Catenable[Int] = largeCatenable.map(_ + 1)
  @Benchmark def mapLargeVector: Vector[Int] = largeVector.map(_ + 1)
  @Benchmark def mapLargeList: List[Int] = largeList.map(_ + 1)

  @Benchmark def foldLeftSmallCatenable: Int = smallCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallVector: Int = smallVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftSmallList: Int = smallList.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeCatenable: Int = largeCatenable.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeVector: Int = largeVector.foldLeft(0)(_ + _)
  @Benchmark def foldLeftLargeList: Int = largeList.foldLeft(0)(_ + _)

  @Benchmark def consSmallCatenable: Catenable[Int] = 0 +: smallCatenable
  @Benchmark def consSmallVector: Vector[Int] = 0 +: smallVector
  @Benchmark def consSmallList: List[Int] = 0 +: smallList
  @Benchmark def consLargeCatenable: Catenable[Int] = 0 +: largeCatenable
  @Benchmark def consLargeVector: Vector[Int] = 0 +: largeVector
  @Benchmark def consLargeList: List[Int] = 0 +: largeList

  @Benchmark def createTinyCatenable: Catenable[Int] = Catenable(1)
  @Benchmark def createTinyVector: Vector[Int] = Vector(1)
  @Benchmark def createTinyList: List[Int] = List(1)
  @Benchmark def createSmallCatenable: Catenable[Int] = Catenable(1, 2, 3, 4, 5)
  @Benchmark def createSmallVector: Vector[Int] = Vector(1, 2, 3, 4, 5)
  @Benchmark def createSmallList: List[Int] = List(1, 2, 3, 4, 5)
}
