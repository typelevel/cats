package cats.tests

import cats.{Contravariant, ContravariantSemigroupal, Invariant, Semigroupal}
import cats.instances.all._
import cats.kernel.{Eq, Hash, Order, PartialOrder}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class KernelContravariantSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  checkAll("Contravariant[Eq]", ContravariantTests[Eq].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Eq]", SemigroupalTests[Eq].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("Contravariant[Eq]", SerializableTests.serializable(Contravariant[Eq]))

  Invariant[PartialOrder]
  Contravariant[PartialOrder]
  Semigroupal[PartialOrder]
  ContravariantSemigroupal[PartialOrder]
  checkAll("Contravariant[PartialOrder]", ContravariantTests[PartialOrder].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[PartialOrder]", SemigroupalTests[PartialOrder].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("Contravariant[PartialOrder]", SerializableTests.serializable(Contravariant[PartialOrder]))

  Invariant[Order]
  Contravariant[Order]
  Semigroupal[Order]
  ContravariantSemigroupal[Order]
  checkAll("Contravariant[Order]", ContravariantTests[Order].contravariant[MiniInt, Int, Boolean])
  checkAll("Semigroupal[Order]", SemigroupalTests[Order].semigroupal[MiniInt, Boolean, Boolean])
  checkAll("Contravariant[Order]", SerializableTests.serializable(Contravariant[Order]))

  Contravariant[Hash]
  checkAll("Contravariant[Hash]", ContravariantTests[Hash].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[Hash]", SerializableTests.serializable(Contravariant[Hash]))
}
