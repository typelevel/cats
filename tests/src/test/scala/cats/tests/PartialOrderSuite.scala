package cats.tests

import cats.{Contravariant, ContravariantMonoidal, Invariant}
import cats.kernel.{Order, PartialOrder}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.syntax.partialOrder._
import cats.tests.Helpers.POrd
import org.scalatest.Assertion

class PartialOrderSuite extends CatsSuite {

  /**
   * Check that two partial compare results are "the same".
   * This works around the fact that `NaN` is not equal to itself.
   */
  def checkPartialCompare(res1: Double, res2: Double): Assertion =
    (res1 == res2 || (res1.isNaN && res2.isNaN)) should ===(true)

  {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }

  checkAll("PartialOrder", ContravariantMonoidalTests[PartialOrder].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[PartialOrder]", SerializableTests.serializable(ContravariantMonoidal[PartialOrder]))

  test("companion object syntax") {
    forAll { (i: Int, j: Int) =>
      val catsKernelStdOrderForInt: Order[Int] = Order[Int]
      checkPartialCompare(PartialOrder.partialCompare(i, j), catsKernelStdOrderForInt.partialCompare(i, j))
      PartialOrder.tryCompare(i, j) should ===(catsKernelStdOrderForInt.tryCompare(i, j))
      PartialOrder.pmin(i, j) should ===(catsKernelStdOrderForInt.pmin(i, j))
      PartialOrder.pmax(i, j) should ===(catsKernelStdOrderForInt.pmax(i, j))
      PartialOrder.lteqv(i, j) should ===(catsKernelStdOrderForInt.lteqv(i, j))
      PartialOrder.lt(i, j) should ===(catsKernelStdOrderForInt.lt(i, j))
      PartialOrder.gteqv(i, j) should ===(catsKernelStdOrderForInt.gteqv(i, j))
      PartialOrder.gt(i, j) should ===(catsKernelStdOrderForInt.gt(i, j))
    }
  }

  test("partial order ops syntax") {
    forAll { (i: POrd, j: POrd) =>
      (i > j) should ===(PartialOrder.gt(i, j))
      (i >= j) should ===(PartialOrder.gteqv(i, j))
      (i < j) should ===(PartialOrder.lt(i, j))
      (i <= j) should ===(PartialOrder.lteqv(i, j))

      checkPartialCompare(i.partialCompare(j), PartialOrder.partialCompare(i, j))
      (i.tryCompare(j)) should ===(PartialOrder.tryCompare(i, j))
      (i.pmin(j)) should ===(PartialOrder.pmin(i, j))
      (i.pmax(j)) should ===(PartialOrder.pmax(i, j))
    }
  }
}

object PartialOrderSuite {
  def summonInstance(): Unit = {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
    ()
  }
}
