package cats
package tests

import cats.data.Xor
import cats.functor.Contravariant
import cats.laws.discipline.{ContravariantTests, FunctorTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.eq.showEq

class FunctorTest extends CatsSuite {
  type OptionXor[A] = Option[Xor[String, A]]

  val optionXorFunctor: Functor[OptionXor] =
    Functor[Option].compose[Xor[String, ?]]
  checkAll("Option compose Xor", FunctorTests(optionXorFunctor).functor[Int, Int, Int])
  checkAll("Functor[Option compose Xor]", SerializableTests.serializable(optionXorFunctor))

  val optionComposeWithFunctorXor: Functor[OptionXor] =
    Functor[Option].composeWithFunctor[Xor[String, ?]]
  checkAll("Option composeWithFunctor Xor", FunctorTests(optionComposeWithFunctorXor).functor[Int, Int, Int])
  checkAll("Functor[Option composeWithFunctor Xor]", SerializableTests.serializable(optionComposeWithFunctorXor))

  type OptionShow[A] = Option[Show[A]]
  val optionShowContravariant: Contravariant[OptionShow] =
      Functor[Option].composeWithContravariant[Show]
  checkAll("Option composeWithContravariant Show", ContravariantTests(optionShowContravariant).contravariant[Int, Int, Int])
  checkAll("Contravariant[Option composeWithContravariant Show]", SerializableTests.serializable(optionShowContravariant))

  test("void replaces values with unit preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int]) =>
      l.void should === (List.fill(l.length)(()))
      o.void should === (if (o.nonEmpty) Some(()) else None)
      m.void should === (m.keys.map(k => (k, ())).toMap)
    }
  }

  test("as replaces values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      l.as(i) should === (List.fill(l.length)(i))
      o.as(i) should === (if (o.nonEmpty) Some(i) else None)
      m.as(i) should === (m.keys.map(k => (k, i)).toMap)
    }
  }
}
