package cats.tests
import cats.arrow.{ArrowChoice, CommutativeArrow}
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.{ArrowChoiceTests, CommutativeArrowTests, MiniInt}

class PartialFunctionSuite extends CatsSuite {

  checkAll("ArrowChoice[PartialFunction]", SerializableTests.serializable(ArrowChoice[PartialFunction]))

  checkAll("PartialFunction",
           ArrowChoiceTests[PartialFunction].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )

  checkAll("CommutativeArrow[PartialFunction]", SerializableTests.serializable(CommutativeArrow[PartialFunction]))
  checkAll(
    "PartialFunction",
    CommutativeArrowTests[PartialFunction].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )

}
