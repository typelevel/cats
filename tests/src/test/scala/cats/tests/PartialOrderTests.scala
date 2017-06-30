package cats
package tests

import cats.functor._

class PartialOrderTests extends CatsSuite {
  {
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
  }

  test("companion object syntax") {
    PartialOrder.partialCompare(1, 2) should ===(catsKernelStdOrderForInt.partialCompare(1, 2))
    PartialOrder.tryCompare(1, 2) should ===(catsKernelStdOrderForInt.tryCompare(1, 2))
    PartialOrder.pmin(1, 2) should ===(catsKernelStdOrderForInt.pmin(1, 2))
    PartialOrder.pmax(1, 2) should ===(catsKernelStdOrderForInt.pmax(1, 2))
    PartialOrder.lteqv(1, 2) should ===(catsKernelStdOrderForInt.lteqv(1, 2))
    PartialOrder.lt(1, 2) should ===(catsKernelStdOrderForInt.lt(1, 2))
    PartialOrder.gteqv(1, 2) should ===(catsKernelStdOrderForInt.gteqv(1, 2))
    PartialOrder.gt(1, 2) should ===(catsKernelStdOrderForInt.gt(1, 2))
  }
}

object PartialOrderTests {
  def summonInstance(): Unit = {
    import cats.instances.partialOrder._
    Invariant[PartialOrder]
    Contravariant[PartialOrder]
    ()
  }
}
