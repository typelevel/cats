package cats
package tests

import cats.arrow.{Arrow, Choice}
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._

class FunctionTests extends CatsSuite {

  checkAll("Function0[Int]", MonoidalTests[Function0].monoidal[Int, Int, Int])
  checkAll("Monoidal[Function0]", SerializableTests.serializable(Monoidal[Function0]))

  checkAll("Function0[Int]", BimonadTests[Function0].bimonad[Int, Int, Int])
  checkAll("Bimonad[Function0]", SerializableTests.serializable(Comonad[Function0]))

  implicit val iso = MonoidalTests.Isomorphisms.covariant[Function1[Int, ?]]
  checkAll("Function1[Int, Int]", MonoidalTests[Function1[Int, ?]].monoidal[Int, Int, Int])
  checkAll("Monoidal[Function1[Int, ?]]", SerializableTests.serializable(Monoidal[Function1[Int, ?]]))

  checkAll("Function1[Int, Int]", MonadReaderTests[Int => ?, Int].monadReader[Int, Int, Int])
  checkAll("MonadReader[Int => ?, Int]", SerializableTests.serializable(MonadReader[Int => ?, Int]))

  checkAll("Function1[Int, Int]", ArrowTests[Function1].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Function1]", SerializableTests.serializable(Arrow[Function1]))

  checkAll("Function1[Int, Int]", ChoiceTests[Function1].choice[Int, Int, Int, Int])
  checkAll("Choice[Function1]", SerializableTests.serializable(Choice[Function1]))
}
