package cats.tests

import catalysts.Platform
import cats.data._

class AndThenSuite extends CatsSuite {
  test("compose a chain of functions with andThen") {
    check { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.andThen(_)).map(_(i))
      val expect = fs.reduceOption(_.andThen(_)).map(_(i))

      result == expect
    }
  }

  test("compose a chain of functions with compose") {
    check { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.compose(_)).map(_(i))
      val expect = fs.reduceOption(_.compose(_)).map(_(i))

      result == expect
    }
  }

  test("andThen is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => { i: Int => i + 1 })
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.andThen(_))(42)

    result shouldEqual (count + 42)
  }

  test("compose is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => { i: Int => i + 1 })
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.compose(_))(42)

    result shouldEqual (count + 42)
  }

  test("Function1 andThen is stack safe") {
    val count = if (Platform.isJvm) 50000 else 1000
    val start: (Int => Int) = AndThen((x: Int) => x)
    val fs = (0 until count).foldLeft(start) { (acc, _) =>
      acc.andThen(_ + 1)
    }
    fs(0) shouldEqual count
  }

  test("toString") {
    AndThen((x: Int) => x).toString should startWith("AndThen$")
  }
}