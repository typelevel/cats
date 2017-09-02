package cats
package tests

import cats.functor._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class KernelContravariantTests extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Cartesian[Eq]
  ContravariantCartesian[Eq]
  checkAll("Contravariant[Eq]", ContravariantTests[Eq].contravariant[Int, Int, Int])
  checkAll("Cartesian[Eq]", CartesianTests[Eq].cartesian[Int, Int, Int])
  checkAll("Contravariant[Eq]", SerializableTests.serializable(Contravariant[Eq]))

  Invariant[PartialOrder]
  Contravariant[PartialOrder]
  Cartesian[PartialOrder]
  ContravariantCartesian[PartialOrder]
  checkAll("Contravariant[PartialOrder]", ContravariantTests[PartialOrder].contravariant[Int, Int, Int])
  checkAll("Cartesian[PartialOrder]", CartesianTests[PartialOrder].cartesian[Int, Int, Int])
  checkAll("Contravariant[PartialOrder]", SerializableTests.serializable(Contravariant[PartialOrder]))

  Invariant[Order]
  Contravariant[Order]
  Cartesian[Order]
  ContravariantCartesian[Order]
  checkAll("Contravariant[Order]", ContravariantTests[Order].contravariant[Int, Int, Int])
  checkAll("Cartesian[Order]", CartesianTests[Order].cartesian[Int, Int, Int])
  checkAll("Contravariant[Order]", SerializableTests.serializable(Contravariant[Order]))
}
