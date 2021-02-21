package cats.tests

import cats.kernel.{BoundedSemilattice, CommutativeGroup, CommutativeMonoid, Hash, Order}
import cats.kernel.laws.discipline._
import cats.laws.discipline.MiniInt
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Gen
import cats.syntax.eq._
import org.scalacheck.Prop._

class MiniIntSuite extends CatsSuite {
  checkAll("MiniInt", OrderTests[MiniInt].order)
  checkAll("Order[MiniInt]", SerializableTests.serializable(Order[MiniInt]))

  checkAll("MiniInt", HashTests[MiniInt].hash)
  checkAll("Hash[MiniInt]", SerializableTests.serializable(Hash[MiniInt]))

  {
    implicit val g: CommutativeGroup[MiniInt] = miniIntAddition
    checkAll("MiniInt addition", CommutativeGroupTests[MiniInt].commutativeGroup)
    checkAll("CommutativeGroup[MiniInt] addition", SerializableTests.serializable(miniIntAddition))
  }

  {
    implicit val m: CommutativeMonoid[MiniInt] = miniIntMultiplication
    checkAll("MiniInt multiplication", CommutativeMonoidTests[MiniInt].commutativeMonoid)
    checkAll("CommutativeMonoid[MiniInt] multiplication", SerializableTests.serializable(miniIntMultiplication))
  }

  {
    implicit val b: BoundedSemilattice[MiniInt] = miniIntOr
    checkAll("MiniInt |", BoundedSemilatticeTests[MiniInt].boundedSemilattice)
    checkAll("BoundedSemilattice[MiniInt] |", SerializableTests.serializable(miniIntOr))
  }

  test("int roundtrip") {
    forAll { (i: MiniInt) =>
      assert(MiniInt.fromInt(i.toInt) === (Some(i)))
    }
  }

  test("int bounds") {
    forAll(Gen.chooseNum(MiniInt.minIntValue, MiniInt.maxIntValue)) { (i: Int) =>
      assert(MiniInt.fromInt(i).map(_.toInt) === (Some(i)))
    }
  }
}
