package cats.tests

import cats.{Contravariant, ContravariantSemigroupal, Decidable, Eq, Invariant, Semigroupal}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  Decidable[Eq]

  checkAll("Eq", DecidableTests[Eq].decidable[MiniInt, MiniInt, MiniInt])
  checkAll("Decidable[Eq]", SerializableTests.serializable(Decidable[Eq]))
}
