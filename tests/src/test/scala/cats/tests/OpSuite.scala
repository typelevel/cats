package cats
package tests

import cats.arrow._
import cats.data.{Kleisli, Op}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.kernel.laws.discipline.EqTests

class OpSuite extends CatsSuite {
  {
    implicit val catsKernelEqForOp = Op.catsKernelEqForOp[Function1, Char, Int]
    checkAll("Op[Function1, Char, Int]", EqTests[Op[Function1, Char, Int]].eqv)
    checkAll("Eq[Op[Function1, Char, Int]]", SerializableTests.serializable(Eq[Op[Function1, Char, Int]]))
  }

  {
    implicit val catsDataCategoryForOp = Op.catsDataCategoryForOp[Function1]
    checkAll("Op[Function1, Char, Int]", CategoryTests[Op[Function1, ?, ?]].category[Char, Int, Char, Int])
    checkAll("Category[Op[Function1, ?, ?]]", SerializableTests.serializable(Category[Op[Function1, ?, ?]]))
  }

  {
    implicit val catsDataDecideableForOp = Op.catsDataDecideableForOp[Function1, Int]
    implicit val coproductIsos = DecideableTests.Isomorphisms.invariant[Op[Function1, Int, ?]]
    checkAll("Op[Function1, Monoid, ?]", DecideableTests[Op[Function1, Int, ?]].decideable[Char, Char, Char])
    checkAll("Decideable[Op[Function1, Monoid, ?]]", SerializableTests.serializable(Decideable[Op[Function1, Int, ?]]))
  }

  {
    implicit val catsDataContravariantMonoidalForOp = Op.catsDataContravariantMonoidalForOp[Function1, Int]
    checkAll("Op[Function1, Monoid, ?]",
             ContravariantMonoidalTests[Op[Function1, Int, ?]].contravariantMonoidal[Char, Char, Char])
    checkAll("ContravariantMonoidal[Op[Function1, Monoid, ?]]",
             SerializableTests.serializable(ContravariantMonoidal[Op[Function1, Int, ?]]))

  }

  {
    implicit val catsDataContravariantForOp = Op.catsDataContravariantForOp[Function1, Unit]
    checkAll("Op[Function1, Unit, ?]", ContravariantTests[Op[Function1, Unit, ?]].contravariant[Char, Char, Char])
    checkAll("Contravariant[Op[Function1, Unit, ?]]",
             SerializableTests.serializable(Contravariant[Op[Function1, Int, ?]]))

  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    // Arr is Function1
    Category[Op[Function1, ?, ?]]
    Compose[Op[Function1, ?, ?]]
    Eq[Op[Function1, Char, Int]]

    // Arr is Kleisli[Option, ?, ?]
    Category[Op[Kleisli[Option, ?, ?], ?, ?]]
    Compose[Op[Kleisli[Option, ?, ?], ?, ?]]
  }
}
