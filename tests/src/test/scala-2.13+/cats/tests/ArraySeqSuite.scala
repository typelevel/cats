package cats
package tests

import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests}
import cats.laws.discipline.{MonadTests, MonoidKTests, SerializableTests, TraverseFilterTests, TraverseTests}
import cats.laws.discipline.arbitrary._

import scala.collection.immutable.ArraySeq

class ArraySeqSuite extends CatsSuite {
  checkAll("ArraySeq[Int]", MonoidTests[ArraySeq[Int]].monoid)
  checkAll("Monoid[ArraySeq]", SerializableTests.serializable(Monoid[ArraySeq[Int]]))

  checkAll("ArraySeq[Int]", OrderTests[ArraySeq[Int]].order)
  checkAll("Order[ArraySeq]", SerializableTests.serializable(Order[ArraySeq[Int]]))

  checkAll("ArraySeq[Int]", MonadTests[ArraySeq].monad[Int, Int, Int])
  checkAll("Monad[ArraySeq]", SerializableTests.serializable(Monad[ArraySeq]))

  checkAll("ArraySeq[Int]", MonoidKTests[ArraySeq].monoidK[Int])
  checkAll("MonoidK[ArraySeq]", SerializableTests.serializable(MonoidK[ArraySeq]))

  checkAll("ArraySeq[Int] with Option", TraverseTests[ArraySeq].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[ArraySeq]", SerializableTests.serializable(Traverse[ArraySeq]))

  checkAll("ArraySeq[Int]", TraverseFilterTests[ArraySeq].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[ArraySeq]", SerializableTests.serializable(TraverseFilter[ArraySeq]))

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("ArraySeq[Int]", EqTests[ArraySeq[ListWrapper[Int]]].eqv)
    checkAll("Eq[ArraySeq]", SerializableTests.serializable(Eq[ArraySeq[ListWrapper[Int]]]))
  }

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("ArraySeq[Int]", PartialOrderTests[ArraySeq[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[ArraySeq]", SerializableTests.serializable(PartialOrder[ArraySeq[ListWrapper[Int]]]))
  }

  {
    implicit val hash: Hash[ListWrapper[Int]] = ListWrapper.hash[Int]
    checkAll("ArraySeq[Int]", HashTests[ArraySeq[ListWrapper[Int]]].hash)
    checkAll("Hash[ArraySeq]", SerializableTests.serializable(Hash[ArraySeq[ListWrapper[Int]]]))
  }

  test("show") {
    ArraySeq(1, 2, 3).show should ===(s"ArraySeq(1, 2, 3)")
    ArraySeq.empty[Int].show should ===(s"ArraySeq()")
  }

  test("MonoidK.algebra consistent with Monoid") {
    forAll { (xs: ArraySeq[Int], ys: ArraySeq[Int]) =>
      MonoidK[ArraySeq].algebra[Int].combine(xs, ys) should ===(Monoid[ArraySeq[Int]].combine(xs, ys))
    }
  }
}
