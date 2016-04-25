package cats
package tests

import cats.arrow.{Arrow, Choice}
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.GroupLaws

class FunctionTests extends CatsSuite {

  checkAll("Function0[Int]", CartesianTests[Function0].cartesian[Int, Int, Int])
  checkAll("Cartesian[Function0]", SerializableTests.serializable(Cartesian[Function0]))

  checkAll("Function0[Int]", BimonadTests[Function0].bimonad[Int, Int, Int])
  checkAll("Bimonad[Function0]", SerializableTests.serializable(Bimonad[Function0]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[Function1[Int, ?]]
  checkAll("Function1[Int, Int]", CartesianTests[Function1[Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Function1[Int, ?]]", SerializableTests.serializable(Cartesian[Function1[Int, ?]]))

  checkAll("Function1[Int, Int]", MonadReaderTests[Int => ?, Int].monadReader[Int, Int, Int])
  checkAll("MonadReader[Int => ?, Int]", SerializableTests.serializable(MonadReader[Int => ?, Int]))

  checkAll("Function1[Int, Int]", ArrowTests[Function1].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Function1]", SerializableTests.serializable(Arrow[Function1]))

  checkAll("Function1[Int, Int]", ChoiceTests[Function1].choice[Int, Int, Int, Int])
  checkAll("Choice[Function1]", SerializableTests.serializable(Choice[Function1]))

  checkAll("Function1[Int, Int]", ContravariantTests[? => Int].contravariant[Int, Int, Int])
  checkAll("Contravariant[? => Int]", SerializableTests.serializable(Contravariant[? => Int]))

  checkAll("Function1[String, Int]", GroupLaws[Function1[String, Int]].semigroup(function1Semigroup[String, Int]))

  checkAll("Function1[String, Int]", GroupLaws[Function1[String, Int]].monoid)

  checkAll("Function1[Int, Int]", MonoidKTests[Lambda[A => A => A]].semigroupK[Int])
  checkAll("SemigroupK[Lambda[A => A => A]", SerializableTests.serializable(function1SemigroupK))

  checkAll("Function1[Int, Int]", MonoidKTests[Lambda[A => A => A]].monoidK[Int])
  checkAll("MonoidK[Lambda[A => A => A]", SerializableTests.serializable(function1MonoidK))
}
