package cats
package tests

import Helpers.POrd

class PartialOrderSuite extends CatsSuite {
  {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }

  test("companion object syntax") {
    forAll { (i: Int, j: Int) =>
      PartialOrder.partialCompare(i, j) should ===(catsKernelStdOrderForInt.partialCompare(i, j))
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

      (i partialCompare j) should ===(PartialOrder.partialCompare(i, j))
      (i tryCompare j) should ===(PartialOrder.tryCompare(i, j))
      (i pmin j) should ===(PartialOrder.pmin(i, j))
      (i pmax j) should ===(PartialOrder.pmax(i, j))
    }
  }
}

object PartialOrderSuite {
  def summonInstance(): Unit = {
    import cats.instances.partialOrder._
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
    ()
  }
}
