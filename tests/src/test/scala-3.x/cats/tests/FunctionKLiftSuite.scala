package cats.tests

import cats.Applicative
import cats.data.NonEmptyList
import cats.arrow.FunctionK
import cats.implicits._
import org.scalacheck.Prop._
import cats.laws.discipline.arbitrary._

class FunctionKLiftSuite extends CatsSuite {
  type OptionOfNel[+A] = Option[NonEmptyList[A]]

  test("lift a polymorphic function directly") {
    val fHeadOption = FunctionK.lift[List, Option]([X] => (_: List[X]).headOption)
    forAll { (a: List[Int]) =>
      assert(fHeadOption(a) === a.headOption)
    }
  }
}
