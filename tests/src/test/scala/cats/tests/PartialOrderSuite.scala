package cats.tests

import cats.{Contravariant, ContravariantMonoidal, Invariant}
import cats.kernel.{Order, PartialOrder}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.implicits._
import cats.tests.Helpers.POrd
import org.scalacheck.Prop._

class PartialOrderSuite extends CatsSuite {

  /**
   * Check that two partial compare results are "the same".
   * This works around the fact that `NaN` is not equal to itself.
   */
  def checkPartialCompare(res1: Double, res2: Double): Unit =
    assert(res1 == res2 || (res1.isNaN && res2.isNaN))

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
      assert(PartialOrder.tryCompare(i, j) === (catsKernelStdOrderForInt.tryCompare(i, j)))
      assert(PartialOrder.pmin(i, j) === (catsKernelStdOrderForInt.pmin(i, j)))
      assert(PartialOrder.pmax(i, j) === (catsKernelStdOrderForInt.pmax(i, j)))
      assert(PartialOrder.lteqv(i, j) === (catsKernelStdOrderForInt.lteqv(i, j)))
      assert(PartialOrder.lt(i, j) === (catsKernelStdOrderForInt.lt(i, j)))
      assert(PartialOrder.gteqv(i, j) === (catsKernelStdOrderForInt.gteqv(i, j)))
      assert(PartialOrder.gt(i, j) === (catsKernelStdOrderForInt.gt(i, j)))
    }
  }

  test("partial order ops syntax") {
    forAll { (i: POrd, j: POrd) =>
      assert((i > j) === (PartialOrder.gt(i, j)))
      assert((i >= j) === (PartialOrder.gteqv(i, j)))
      assert((i < j) === (PartialOrder.lt(i, j)))
      assert((i <= j) === (PartialOrder.lteqv(i, j)))

      checkPartialCompare(i.partialCompare(j), PartialOrder.partialCompare(i, j))
      assert(i.partialComparison(j) === (PartialOrder[POrd].partialComparison(i, j)))
      assert(i.tryCompare(j) === (PartialOrder.tryCompare(i, j)))
      assert(i.pmin(j) === (PartialOrder.pmin(i, j)))
      assert(i.pmax(j) === (PartialOrder.pmax(i, j)))
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
