package cats.tests

import cats.{Id, MonadError}
import cats.data.{Xor, XorT}
import cats.laws.discipline.{BifunctorTests, MonadErrorTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._


class XorTTests extends CatsSuite {
  checkAll("XorT[List, String, Int]", MonadErrorTests[XorT[List, String, ?], String].monadError[Int, Int, Int])
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("MonadError[XorT[List, ?, ?]]", SerializableTests.serializable(MonadError[XorT[List, String, ?], String]))
  checkAll("XorT[List, ?, ?]", BifunctorTests[XorT[List, ?, ?]].bifunctor[Int, Int, Int, String, String, String])

  test("toValidated") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toValidated.map(_.toXor) should === (xort.value)
    }
  }

  test("withValidated") {
    forAll { (xort: XorT[List, String, Int], f: String => Char, g: Int => Double) =>
      xort.withValidated(_.bimap(f, g)) should === (xort.bimap(f, g))
    }
  }

  test("fromXor") {
    forAll { (xor: Xor[String, Int]) =>
      Some(xor.isLeft) should === (XorT.fromXor[Option](xor).isLeft)
    }
  }

  test("isLeft negation of isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.isLeft should === (xort.isRight.map(! _))
    }
  }

  test("double swap is noop") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.swap should === (xort)
    }
  }

  test("swap negates isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.isRight should === (xort.isRight.map(! _))
    }
  }

  test("toOption on Right returns Some") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toOption.map(_.isDefined) should === (xort.isRight)
    }
  }

  test("toEither preserves isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toEither.map(_.isRight) should === (xort.isRight)
    }
  }

  test("recover recovers handled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recover { case "xort" => 5 }.isRight should === (true)
  }

  test("recover ignores unhandled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recover { case "notxort" => 5 } should === (xort)
  }

  test("recover ignores the right side") {
    val xort = XorT.right[Id, String, Int](10)
    xort.recover { case "xort" => 5 } should === (xort)
  }

  test("recoverWith recovers handled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recoverWith { case "xort" => XorT.right[Id, String, Int](5) }.isRight should === (true)
  }

  test("recoverWith ignores unhandled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recoverWith { case "notxort" => XorT.right[Id, String, Int](5) } should === (xort)
  }

  test("recoverWith ignores the right side") {
    val xort = XorT.right[Id, String, Int](10)
    xort.recoverWith { case "xort" => XorT.right[Id, String, Int](5) } should === (xort)
  }
}
