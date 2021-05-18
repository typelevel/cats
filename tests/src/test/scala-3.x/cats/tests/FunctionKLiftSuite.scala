package cats.tests

import cats.arrow.FunctionK
import cats.implicits._
import org.scalacheck.Prop._
import cats.laws.discipline.arbitrary._

class FunctionKLiftSuite extends CatsSuite {

  test("lift a polymorphic function directly") {
    val fHeadOption = FunctionK.lift[List, Option]([X] => (_: List[X]).headOption)
    forAll { (a: List[Int]) =>
      assert(fHeadOption(a) === a.headOption)
    }
  }

  test("FunctionK[F, G] extends [X] => F[X] => G[X]") {
    val poly: [X] => List[X] => List[X] = FunctionK.id[List]
  }
}
