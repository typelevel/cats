package cats
package tests

import algebra.laws.OrderLaws

import cats.data.Streaming
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class StreamingTests extends CatsSuite {
  checkAll("Streaming[Int]", CoflatMapTests[Streaming].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Streaming]", SerializableTests.serializable(CoflatMap[Streaming]))

  checkAll("Streaming[Int]", MonadCombineTests[Streaming].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Streaming]", SerializableTests.serializable(MonadCombine[Streaming]))

  checkAll("Streaming[Int] with Option", TraverseTests[Streaming].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Streaming]", SerializableTests.serializable(Traverse[Streaming]))

  checkAll("Streaming[Int]", OrderLaws[Streaming[Int]].order)
  checkAll("Order[Streaming[Int]]", SerializableTests.serializable(Order[Streaming[Int]]))
}

class AdHocStreamingTests extends CatsSuite {

  test("results aren't reevaluated after memoize") {
    forAll { (orig: Streaming[Int]) =>
      val ns = orig.toList
      val size = ns.size

      var i = 0
      val memoized = orig.map { n => i += 1; n }.memoize

      val xs = memoized.toList
      i should === (size)
      xs should === (ns)

      val ys = memoized.toList
      i should === (size)
      ys should === (ns)
    }
  }

  // convert List[A] to Streaming[A]
  def convert[A](as: List[A]): Streaming[A] =
    Streaming.fromList(as)

  test("fromList/toList") {
    forAll { (xs: List[Int]) =>
      convert(xs).toList should === (xs)
    }
  }

  // test that functions over Streaming[A] vs List[A] produce the same B.
  def test[A, B](xs: List[A])(f: Streaming[A] => B)(g: List[A] => B): Unit =
    f(convert(xs)) shouldBe g(xs)

  test("map") {
    forAll { (xs: List[Int], f: Int => Double) =>
      test(xs)(_.map(f).toList)(_.map(f))
    }
  }

  // convert (A => List[B]) to (A => Streaming[B])
  def convertF[A, B](f: A => List[B]): A => Streaming[B] =
    (a: A) => Streaming.fromList(f(a))

  test("flatMap") {
    forAll { (xs: List[Int], f: Int => List[Double]) =>
      test(xs)(_.flatMap(convertF(f)).toList)(_.flatMap(f))
    }
  }

  test("filter") {
    forAll { (xs: List[Int], f: Int => Boolean) =>
      test(xs)(_.filter(f).toList)(_.filter(f))
    }
  }

  test("foldLeft") {
    forAll { (xs: List[String], n: Int, f: (Int, String) => Int) =>
      test(xs)(_.foldLeft(n)(f))(_.foldLeft(n)(f))
    }
  }

  test("isEmpty") {
    forAll { (xs: List[String], n: Int, f: (Int, String) => Int) =>
      test(xs)(_.isEmpty)(_.isEmpty)
    }
  }

  test("++") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      (convert(xs) ++ convert(ys)).toList shouldBe (xs ::: ys)
    }
  }

  test("zip") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      (convert(xs) zip convert(ys)).toList shouldBe (xs zip ys)
    }
  }

  test("zipWithIndex") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      test(xs)(_.zipWithIndex.toList)(_.zipWithIndex)
    }
  }

  test("unzip") {
    forAll { (xys: List[(Int, Int)]) =>
      test(xys) { s =>
        val (xs, ys): (Streaming[Int], Streaming[Int]) = s.unzip
        (xs.toList, ys.toList)
      }(_.unzip)
    }
  }

  test("exists") {
    forAll { (xs: List[Int], f: Int => Boolean) =>
      test(xs)(_.exists(f))(_.exists(f))
    }
  }

  test("forall") {
    forAll { (xs: List[Int], f: Int => Boolean) =>
      test(xs)(_.forall(f))(_.forall(f))
    }
  }

  test("take") {
    forAll { (xs: List[Int], n: Int) =>
      test(xs)(_.take(n).toList)(_.take(n))
    }
  }

  test("drop") {
    forAll { (xs: List[Int], n: Int) =>
      test(xs)(_.drop(n).toList)(_.drop(n))
    }
  }

  test("takeWhile") {
    forAll { (xs: List[Int], f: Int => Boolean) =>
      test(xs)(_.takeWhile(f).toList)(_.takeWhile(f))
    }
  }

  test("dropWhile") {
    forAll { (xs: List[Int], f: Int => Boolean) =>
      test(xs)(_.dropWhile(f).toList)(_.dropWhile(f))
    }
  }

  test("tails") {
    forAll { (xs: List[Int]) =>
      test(xs)(_.tails.map(_.toList).toList)(_.tails.toList)
    }
  }

  test("merge") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      (convert(xs.sorted) merge convert(ys.sorted)).toList shouldBe (xs ::: ys).sorted
    }
  }

  test("product") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      val result = (convert(xs) product convert(ys)).iterator.toSet
      val expected = (for { x <- xs; y <- ys } yield (x, y)).toSet
      result shouldBe expected
    }

    val nats = Streaming.from(1) // 1, 2, 3, ...

    def isRelativelyPrime(t: (Int, Int)): Boolean = {
      def euclid(x: Int, y: Int): Int = if (y == 0) x else euclid(y, x % y)
      euclid(t._1, t._2) == 1
    }

    val positiveRationals = (nats product nats).filter(isRelativelyPrime)
    val e = Set((1,1), (2,1), (1,2), (3,1), (1,3), (4,1), (3,2), (2,3), (1,4))
    positiveRationals.take(e.size).iterator.toSet shouldBe e
  }

  test("interleave") {
    forAll { (xs: Vector[Int]) =>
      // not a complete test but it'll have to do for now
      val s = Streaming.fromVector(xs)
      val r = (s interleave s).iterator.toVector
      for (i <- 0 until xs.length) {
        r(i * 2) shouldBe xs(i)
        r(i * 2 + 1) shouldBe xs(i)
      }
    }
  }

  import scala.util.Try

  val bomb: Streaming[Int] =
    Streaming.defer(sys.error("boom"))

  val dangerous: Streaming[Int] =
    1 %:: 2 %:: 3 %:: bomb

  val veryDangerous: Streaming[Int] =
    1 %:: bomb

  test("lazy uncons") {
    veryDangerous.uncons.map(_._1) shouldBe Some(1)
  }

  def isok[U](body: => U): Unit =
    Try(body).isSuccess shouldBe true

  test("lazy map") {
    isok(bomb.map(_ + 1))
  }

  test("lazy flatMap") {
    isok(bomb.flatMap(n => Streaming(n, n)))
  }

  test("lazy filter") {
    isok(bomb.filter(_ > 10))
  }

  test("lazy foldRight") {
    isok(bomb.foldRight(Now(0))((x, total) => total.map(_ + x)))
  }

  test("lazy peekEmpty") {
    bomb.peekEmpty shouldBe None
  }

  test("lazy ++") {
    isok(bomb ++ bomb)
  }

  test("lazier ++") {
    isok(bomb ++ Always(sys.error("ouch"): Streaming[Int]))
  }

  test("lazy zip") {
    isok(bomb zip dangerous)
    isok(dangerous zip bomb)
  }

  test("lazy zipWithIndex") {
    isok(bomb.zipWithIndex)
  }

  test("lazy izip") {
    isok(bomb izip dangerous)
    isok(dangerous izip bomb)
  }

  test("lazy unzip") {
    val bombBomb: Streaming[(Int, Int)] = bomb.map(n => (n, n))
    isok { val t: (Streaming[Int], Streaming[Int]) = bombBomb.unzip }
  }

  test("lazy merge") {
    isok(bomb merge bomb)
  }

  test("lazy interleave") {
    isok(bomb interleave bomb)
  }

  test("lazy product") {
    isok(bomb product bomb)
  }

  test("lazy take") {
    isok(bomb.take(10))
    isok(bomb.take(0))
  }

  test("take up to the last valid element"){
    isok(dangerous.take(3).toList)
  }

  test("lazy drop") {
    isok(bomb.drop(10))
    isok(bomb.drop(0))
  }

  test("lazy takeWhile") {
    isok(bomb.takeWhile(_ < 10))
  }

  test("lazy dropWhile") {
    isok(bomb.takeWhile(_ < 10))
  }

  test("lazy tails") {
    isok(bomb.tails)
  }
}
