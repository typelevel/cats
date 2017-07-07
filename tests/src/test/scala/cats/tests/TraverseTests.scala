package cats
package tests

import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary

import cats.instances.all._

abstract class TraverseCheck[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite with PropertyChecks {

  test(s"Traverse[$name].indexed") {
    forAll { (fa: F[Int]) =>
      fa.indexed.toList should === (fa.toList.zipWithIndex)
    }
  }

}

class TraverseListCheck extends TraverseCheck[List]("list")
class TraverseStreamCheck extends TraverseCheck[Stream]("stream")
class TraverseVectorCheck extends TraverseCheck[Vector]("vector")

class TraverseTestsAdditional extends CatsSuite {

  def checkIndexedStackSafety[F[_]](fromRange: Range => F[Int])(implicit F: Traverse[F]): Unit = {
    F.indexed(fromRange(1 to 70000))
    ()
  }

  test("Traverse[List].indexed stack safety") {
    checkIndexedStackSafety[List](_.toList)
  }

  test("Traverse[Stream].indexed stack safety") {
    checkIndexedStackSafety[Stream](_.toStream)
  }

  test("Traverse[Vector].indexed stack safety") {
    checkIndexedStackSafety[Vector](_.toVector)
  }
}
