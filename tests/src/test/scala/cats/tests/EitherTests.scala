package cats
package tests

import cats.laws.discipline.{BitraverseTests, TraverseTests, MonadTests, SerializableTests, CartesianTests}
import cats.kernel.laws.OrderLaws

class EitherTests extends CatsSuite {

  implicit val iso = CartesianTests.Isomorphisms.invariant[Either[Int, ?]]

  checkAll("Either[Int, Int]", CartesianTests[Either[Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Either[Int, ?]]", SerializableTests.serializable(Cartesian[Either[Int, ?]]))

  checkAll("Either[Int, Int]", MonadTests[Either[Int, ?]].monad[Int, Int, Int])
  checkAll("Monad[Either[Int, ?]]", SerializableTests.serializable(Monad[Either[Int, ?]]))

  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, ?]", SerializableTests.serializable(Traverse[Either[Int, ?]]))

  checkAll("Either[?, ?]", BitraverseTests[Either].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Either]", SerializableTests.serializable(Bitraverse[Either]))

  val partialOrder = eitherPartialOrder[Int, String]
  val order = implicitly[Order[Either[Int, String]]]
  val monad = implicitly[Monad[Either[Int, ?]]]
  val show = implicitly[Show[Either[Int, String]]]

  {
    implicit val S = ListWrapper.eqv[String]
    implicit val I = ListWrapper.eqv[Int]
    checkAll("Either[ListWrapper[String], ListWrapper[Int]]", OrderLaws[Either[ListWrapper[String], ListWrapper[Int]]].eqv)
    checkAll("Eq[Either[ListWrapper[String], ListWrapper[Int]]]", SerializableTests.serializable(Eq[Either[ListWrapper[String], ListWrapper[Int]]]))
  }

  val orderLaws = OrderLaws[Either[Int, String]]
  checkAll("Either[Int, String]", orderLaws.partialOrder(partialOrder))
  checkAll("Either[Int, String]", orderLaws.order(order))


  test("implicit instances resolve specifically") {
    val eq = eitherEq[Int, String]
    assert(!eq.isInstanceOf[PartialOrder[_]])
    assert(!eq.isInstanceOf[Order[_]])
    assert(!partialOrder.isInstanceOf[Order[_]])
  }

  test("show isn't empty") {
    forAll { (e: Either[Int, String]) =>
      show.show(e).nonEmpty should === (true)
    }
  }
}
