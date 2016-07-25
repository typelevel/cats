package cats
package tests

import cats.functor.Contravariant
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ContravariantTests, SerializableTests}
import cats.laws.discipline.eq._

class KernelContravariantTests extends CatsSuite {
  checkAll("Contravariant[Eq]", ContravariantTests[Eq].contravariant[Int, Int, Int])
  checkAll("Contravariant[Eq]", SerializableTests.serializable(Contravariant[Eq]))

  checkAll("Contravariant[PartialOrder]", ContravariantTests[PartialOrder].contravariant[Int, Int, Int])
  checkAll("Contravariant[PartialOrder]", SerializableTests.serializable(Contravariant[PartialOrder]))

  checkAll("Contravariant[Order]", ContravariantTests[Order].contravariant[Int, Int, Int])
  checkAll("Contravariant[Order]", SerializableTests.serializable(Contravariant[Order]))
}
