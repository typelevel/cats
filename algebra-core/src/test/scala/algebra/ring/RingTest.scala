package algebra.ring

import algebra.instances.bigInt._

import org.scalacheck.Prop._

class RingTest extends munit.DisciplineSuite {
  test("Ring.defaultFromBigInt") {
    forAll { (n: BigInt) =>
      assertEquals(Ring.defaultFromBigInt[BigInt](n), n)
    }
  }
}
