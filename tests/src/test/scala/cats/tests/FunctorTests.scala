package cats
package tests

import cats.data.Xor
import cats.functor.Contravariant
import cats.laws.discipline.{ContravariantTests, FunctorTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.showEq

class FunctorTest extends CatsSuite {
  type OptionXor[A] = Option[Xor[String, A]]

  val optionXorFunctor: Functor[OptionXor] =
    Functor[Option].compose[Xor[String, ?]]
  checkAll("Option compose Xor", FunctorTests(optionXorFunctor).functor[Int, Int, Int])
  checkAll("Functor[Option compose Xor]", SerializableTests.serializable(optionXorFunctor))

  type OptionShow[A] = Option[Show[A]]
  val optionShowContravariant: Contravariant[OptionShow] =
      Functor[Option].composeWithContravariant[Show]
  checkAll("Option composeWithContravariant Show", ContravariantTests(optionShowContravariant).contravariant[Int, Int, Int])
  checkAll("Contravariant[Option composeWithContravariant Show]", SerializableTests.serializable(optionShowContravariant))
}
