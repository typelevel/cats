package cats
package tests

import cats.laws.discipline.{MonadTests, MonoidKTests, SerializableTests, TraverseFilterTests, TraverseTests}
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.discipline.{MonoidTests, OrderTests}

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

  test("show") {
    ArraySeq(1, 2, 3).show should ===(s"ArraySeq(1, 2, 3)")
    ArraySeq.empty[Int].show should ===(s"ArraySeq()")
  }
}
