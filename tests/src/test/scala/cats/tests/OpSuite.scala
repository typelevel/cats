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
    implicit val catsKernelEqForOp = Op.catsKernelEqForOp[Function1, Int, MiniInt]
    checkAll("Op[Function1, Int, MiniInt]", EqTests[Op[Function1, Int, MiniInt]].eqv)
    checkAll("Eq[Op[Function1, Int, MiniInt]]", SerializableTests.serializable(Eq[Op[Function1, Int, MiniInt]]))
  }

  {
    implicit val catsDataCategoryForOp = Op.catsDataCategoryForOp[Function1]
    checkAll("Op[Function1, ?, ?]", CategoryTests[Op[Function1, ?, ?]].category[Char, MiniInt, Char, Boolean])
    checkAll("Category[Op[Function1, ?, ?]]", SerializableTests.serializable(Category[Op[Function1, ?, ?]]))
  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    // Arr is Function1
    Category[Op[Function1, ?, ?]]
    Compose[Op[Function1, ?, ?]]
    Eq[Op[Function1, Char, MiniInt]]

    // Arr is Kleisli[Option, ?, ?]
    Category[Op[Kleisli[Option, ?, ?], ?, ?]]
    Compose[Op[Kleisli[Option, ?, ?], ?, ?]]
  }
}
