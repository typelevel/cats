package cats
package tests

import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary

import cats.instances.all._

abstract class TraverseCheck[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite with PropertyChecks {

  test(s"Traverse[$name].zipWithIndex") {
    forAll { (fa: F[Int]) =>
      fa.zipWithIndex.toList should === (fa.toList.zipWithIndex)
    }
  }

  test(s"Traverse[$name].mapWithIndex") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => Int) =>
      fa.mapWithIndex((a, i) => fn((a, i))).toList should === (fa.toList.zipWithIndex.map(fn))
    }
  }

  test(s"Traverse[$name].traverseWithIndex") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => (Int, Int)) =>
      val left = fa.traverseWithIndex((a, i) => fn((a, i))).map(_.toList)
      val (xs, values) = fa.toList.zipWithIndex.map(fn).unzip
      left should === ((xs.combineAll, values))
    }
  }

}

class TraverseListCheck extends TraverseCheck[List]("list")
class TraverseStreamCheck extends TraverseCheck[Stream]("stream")
class TraverseVectorCheck extends TraverseCheck[Vector]("vector")

class TraverseTestsAdditional extends CatsSuite {

  def checkZipWithIndexedStackSafety[F[_]](fromRange: Range => F[Int])(implicit F: Traverse[F]): Unit = {
    F.zipWithIndex(fromRange(1 to 70000))
    ()
  }

  test("Traverse[List].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[List](_.toList)
  }

  test("Traverse[Stream].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Stream](_.toStream)
  }

  test("Traverse[Vector].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Vector](_.toVector)
  }
}
