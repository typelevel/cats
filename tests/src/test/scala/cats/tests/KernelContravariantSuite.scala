package cats
package tests


import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class KernelContravariantSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  checkAll("Contravariant[Eq]", ContravariantTests[Eq].contravariant[Int, Int, Int])
  checkAll("Semigroupal[Eq]", SemigroupalTests[Eq].semigroupal[Int, Int, Int])
  checkAll("Contravariant[Eq]", SerializableTests.serializable(Contravariant[Eq]))

  Invariant[PartialOrder]
  Contravariant[PartialOrder]
  Semigroupal[PartialOrder]
  ContravariantSemigroupal[PartialOrder]
  checkAll("Contravariant[PartialOrder]", ContravariantTests[PartialOrder].contravariant[Int, Int, Int])
  checkAll("Semigroupal[PartialOrder]", SemigroupalTests[PartialOrder].semigroupal[Int, Int, Int])
  checkAll("Contravariant[PartialOrder]", SerializableTests.serializable(Contravariant[PartialOrder]))

  Invariant[Order]
  Contravariant[Order]
  Semigroupal[Order]
  ContravariantSemigroupal[Order]
  checkAll("Contravariant[Order]", ContravariantTests[Order].contravariant[Int, Int, Int])
  checkAll("Semigroupal[Order]", SemigroupalTests[Order].semigroupal[Int, Int, Int])
  checkAll("Contravariant[Order]", SerializableTests.serializable(Contravariant[Order]))

  Contravariant[Hash]
  checkAll("Contravariant[Hash]", ContravariantTests[Hash].contravariant[Int, Int, Int])
  checkAll("Contravariant[Hash]", SerializableTests.serializable(Contravariant[Hash]))
}
