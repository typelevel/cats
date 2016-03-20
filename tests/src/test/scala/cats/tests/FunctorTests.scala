package cats
package tests

import cats.functor.Contravariant
import cats.laws.discipline.{ContravariantTests, FunctorTests, SerializableTests}
import cats.laws.discipline.arbitrary.showArbitrary
import cats.laws.discipline.eq.showEq

class FunctorTest extends CatsSuite {
  type OptionList[A] = Option[List[A]]

  val optionListFunctor: Functor[OptionList] =
    Functor[Option].compose[List]
  checkAll("Option compose List", FunctorTests(optionListFunctor).functor[Int, Int, Int])
  checkAll("Functor[Option compose List]", SerializableTests.serializable(optionListFunctor))

  type OptionShow[A] = Option[Show[A]]
  val optionShowContravariant: Contravariant[OptionShow] =
      Functor[Option].composeWithContravariant[Show]
  checkAll("Option composeWithContravariant Show", ContravariantTests(optionShowContravariant).contravariant[Int, Int, Int])
  checkAll("Contravariant[Option composeWithContravariant Show]", SerializableTests.serializable(optionShowContravariant))
}
