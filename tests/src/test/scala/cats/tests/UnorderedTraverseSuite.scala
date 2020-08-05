package cats.tests

import cats.Id
import cats.syntax.unorderedTraverse._
import cats.syntax.eq._
import org.scalacheck.Prop._

class UnorderedTraverseSuite extends CatsSuite {
  test("UnorderedTraverse[Set[Int]].unorderedTraverse via syntax") {
    forAll { (ins: Set[Int]) =>
      assert(ins.unorderedTraverse(in => in: Id[Int]).toList.sorted === (ins.toList.sorted))
    }
  }
}
