package cats
package tests



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
}

object PartialOrderSuite {
  def summonInstance(): Unit = {
    import cats.instances.partialOrder._
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
    ()
  }
}
