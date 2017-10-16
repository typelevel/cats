package cats
package tests

import cats.laws.discipline.{FoldableTests, MonoidKTests, SerializableTests}
import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests}
class SetTests extends CatsSuite {
  checkAll("Set[Int]", MonoidLawTests[Set[Int]].monoid)

  checkAll("Set[Int]", MonoidKTests[Set].monoidK[Int])
  checkAll("MonoidK[Set]", SerializableTests.serializable(MonoidK[Set]))

  checkAll("Set[Int]", FoldableTests[Set].foldable[Int, Int])
  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))

  test("show"){
    Set(1, 1, 2, 3).show should === ("Set(1, 2, 3)")
    Set.empty[String].show should === ("Set()")

    forAll { fs: Set[String] =>
      fs.show should === (fs.toString)
    }
  }

  test("show keeps separate entries for items that map to identical strings"){
    //note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = Show.show(_ => "1")
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because Set.map combines
    // duplicate items in the codomain.
    Set(1, 2, 3).show should === ("Set(1, 1, 1)")
  }
}
