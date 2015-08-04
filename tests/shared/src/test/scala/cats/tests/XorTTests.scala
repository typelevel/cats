package cats.tests

import cats.MonadError
import cats.data.{Xor, XorT}
import cats.laws.discipline.{MonadErrorTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

import org.scalacheck.Prop.forAll

class XorTTests extends CatsSuite {
  checkAll("XorT[List, String, Int]", MonadErrorTests[XorT[List, ?, ?], String].monadError[Int, Int, Int])
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("MonadError[XorT[List, ?, ?]]", SerializableTests.serializable(MonadError[XorT[List, ?, ?], String]))

  test("toValidated")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toValidated.map(_.toXor) == xort.value
    }
  })

  test("withValidated")(check {
    forAll { (xort: XorT[List, String, Int], f: String => Char, g: Int => Double) =>
      xort.withValidated(_.bimap(f, g)) == xort.bimap(f, g)
    }
  })

  test("fromXor")(check {
    forAll { (xor: Xor[String, Int]) =>
      Some(xor.isLeft) == XorT.fromXor[Option](xor).isLeft
    }
  })

  test("isLeft negation of isRight")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.isLeft == xort.isRight.map(! _)
    }
  })

  test("double swap is noop")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.swap === xort
    }
  })

  test("swap negates isRight")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.isRight == xort.isRight.map(! _)
    }
  })

  test("toOption on Right returns Some")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toOption.map(_.isDefined) == xort.isRight
    }
  })

  test("toEither preserves isRight")(check {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toEither.map(_.isRight) == xort.isRight
    }
  })
}
