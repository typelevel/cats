package cats.tests
import cats.arrow.{ArrowChoice, Choice, CommutativeArrow}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.{ArrowChoiceTests, ChoiceTests, CommutativeArrowTests, MiniInt}

class PartialFunctionSuite extends CatsSuite {
  checkAll(
    "PartialFunction",
    CommutativeArrowTests[PartialFunction].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )
  checkAll("Arrow[PartialFunction]", SerializableTests.serializable(CommutativeArrow[PartialFunction]))

  checkAll("PartialFunction", ChoiceTests[PartialFunction].choice[MiniInt, Boolean, Int, Long])
  checkAll("Choice[PartialFunction]", SerializableTests.serializable(Choice[PartialFunction]))

  checkAll("PartialFunction",
           ArrowChoiceTests[PartialFunction].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt])
  checkAll("ArrowChoice[PartialFunction]", SerializableTests.serializable(ArrowChoice[PartialFunction]))

}
