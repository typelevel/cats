package cats
package tests

import cats.laws.discipline.{TraverseTests, MonadTests, SerializableTests}
import org.scalacheck.Prop._

class EitherTests extends CatsSuite {
  checkAll("Either[Int, Int]", MonadTests[Either[Int, ?]].flatMap[Int, Int, Int])
  checkAll("Monad[Either[Int, ?]]", SerializableTests.serializable(Monad[Either[Int, ?]]))

  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, ?]", SerializableTests.serializable(Traverse[Either[Int, ?]]))

  val eq = eitherEq[Int, String]
  val partialOrder = eitherPartialOrder[Int, String]
  val order = implicitly[Order[Either[Int, String]]]
  val monad = implicitly[Monad[Either[Int, ?]]]
  val show = implicitly[Show[Either[Int, String]]]


  test("implicit instances resolve specifically") {
    assert(!eq.isInstanceOf[PartialOrder[_]])
    assert(!eq.isInstanceOf[Order[_]])
    assert(!partialOrder.isInstanceOf[Order[_]])
  }

  check {
    forAll { (e: Either[Int, String]) =>
      eq.eqv(e, e)
    }
  }

  check {
    forAll { (e: Either[Int, String]) =>
      partialOrder.partialCompare(e, e) == 0.0
    }
  }

  check {
    forAll { (e: Either[Int, String]) =>
      order.compare(e, e) == 0
    }
  }

  check {
    forAll { (e: Either[Int, String], f: Either[Int, String]) =>
      eq.eqv(e, f) == partialOrder.eqv(e, f) &&
      eq.eqv(e, f) == order.eqv(e, f)
    }
  }

  check {
    forAll { (e: Either[Int, String], f: Either[Int, String]) =>
      partialOrder.partialCompare(e, f) == order.partialCompare(e, f)
    }
  }

  check {
    forAll { (e: Either[Int, String], f: Either[Int, String]) =>
      !partialOrder.partialCompare(e, f).isNaN
    }
  }

  check {
    forAll { (e: Either[Int, String], f: Either[Int, String]) =>
      partialOrder.partialCompare(e,f).toInt == order.compare(e, f)
      partialOrder.partialCompare(e,f).toInt == order.compare(e, f)
    }
  }

  check {
    forAll { (e: Either[Int, String]) =>
      show.show(e).nonEmpty
    }
  }

  check {
    forAll { (s: String, f: String => Int) =>
      monad.map(monad.pure(s))(f) == monad.pure(f(s))
    }
  }
}
