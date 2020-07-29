package cats.tests

import cats.{Contravariant, ContravariantMonoidal, Decidable}
import cats.arrow._
import cats.data.{Kleisli, Op}
import cats.kernel.Eq
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.kernel.laws.discipline.EqTests

class OpSuite extends CatsSuite {
  {
    implicit val catsDataEqForOp: Eq[Op[Function1, Int, MiniInt]] = Op.catsDataEqForOp[Function1, Int, MiniInt]
    checkAll("Op[Function1, Int, MiniInt]", EqTests[Op[Function1, Int, MiniInt]].eqv)
    checkAll("Eq[Op[Function1, Int, MiniInt]]", SerializableTests.serializable(Eq[Op[Function1, Int, MiniInt]]))
  }

  {
    implicit val catsDataCategoryForOp: Category[Op[Function1, *, *]] = Op.catsDataCategoryForOp[Function1]
    checkAll("Op[Function1, *, *]", CategoryTests[Op[Function1, *, *]].category[Char, MiniInt, Char, Boolean])
    checkAll("Category[Op[Function1, *, *]]", SerializableTests.serializable(Category[Op[Function1, *, *]]))
  }

  {
    implicit val catsDataDecidableForOp = Op.catsDataDecidableForOp[Function1, Int]
    checkAll("Op[Function1, Monoid, ?]", DecidableTests[Op[Function1, Int, *]].decidable[MiniInt, MiniInt, MiniInt])
    checkAll("Decidable[Op[Function1, Monoid, ?]]", SerializableTests.serializable(Decidable[Op[Function1, Int, *]]))
  }

  {
    implicit val catsDataContravariantMonoidalForOp = Op.catsDataContravariantMonoidalForOp[Function1, Int]
    checkAll("Op[Function1, Monoid, ?]",
             ContravariantMonoidalTests[Op[Function1, Int, *]].contravariantMonoidal[MiniInt, MiniInt, MiniInt]
    )
    checkAll("ContravariantMonoidal[Op[Function1, Monoid, ?]]",
             SerializableTests.serializable(ContravariantMonoidal[Op[Function1, Int, *]])
    )

  }

  {
    implicit val catsDataContravariantForOp = Op.catsDataContravariantMonoidalForOp[Function1, Unit]
    checkAll("Op[Function1, Unit, ?]",
             ContravariantTests[Op[Function1, Unit, *]].contravariant[MiniInt, MiniInt, MiniInt]
    )
    checkAll("Contravariant[Op[Function1, Unit, ?]]",
             SerializableTests.serializable(Contravariant[Op[Function1, Int, *]])
    )

  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    // Arr is Function1
    Category[Op[Function1, *, *]]
    Compose[Op[Function1, *, *]]
    Eq[Op[Function1, Char, MiniInt]]

    // Arr is Kleisli[Option, *, *]
    Category[Op[Kleisli[Option, *, *], *, *]]
    Compose[Op[Kleisli[Option, *, *], *, *]]
  }
}
