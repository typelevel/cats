package cats.tests

import cats.{MonoidK, Show, UnorderedTraverse}
import cats.data.Validated
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{MonoidKTests, SerializableTests, UnorderedTraverseTests}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._

class SetSuite extends CatsSuite {
  checkAll("Set[Int]", MonoidTests[Set[Int]].monoid)

  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
  checkAll("MonoidK[Set]", SerializableTests.serializable(MonoidK[Set]))

  checkAll("Set[Int]", UnorderedTraverseTests[Set].unorderedTraverse[Int, Int, Int, Validated[Int, *], Option])
  checkAll("UnorderedTraverse[Set]", SerializableTests.serializable(UnorderedTraverse[Set]))

  test("show") {
    assert(Set(1, 1, 2, 3).show === ("Set(1, 2, 3)"))
    assert(Set.empty[String].show === ("Set()"))
  }

  test("show keeps separate entries for items that map to identical strings") {
    // note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = Show.show(_ => "1")
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because Set.map combines
    // duplicate items in the codomain.
    assert(Set(1, 2, 3).show === ("Set(1, 1, 1)"))
  }
}
