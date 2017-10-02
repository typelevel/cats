package cats
package tests

import cats.data.{Nested, Tuple2K}
import cats.laws.discipline.{FoldableTests, MonoidKTests, SerializableTests}

class SetTests extends CatsSuite {
  checkAll("Set[Int]", cats.kernel.laws.GroupLaws[Set[Int]].monoid)

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

  test("traverseUnordered identity") {
    forAll { (si: Set[Int], f: Int => String) =>
      CommutativeApplicative.traverseUnordered[Id, Int, String](si)(f) should === (si.map(f))
    }
  }

  test("traverseUnordered sequential composition") {
    forAll { (si: Set[Int], f: Int => Option[String], g: String => Option[Int]) =>
      val lhs = Nested(CommutativeApplicative.traverseUnordered(si)(f).map(ss => CommutativeApplicative.traverseUnordered(ss)(g)))
      val rhs = CommutativeApplicative.traverseUnordered(si)(i => Nested(f(i).map(g)))
      lhs should === (rhs)
    }
  }

  test("traverseUnordered parallel composition") {
    forAll { (si: Set[Int], f: Int => Option[String], g: Int => Option[String]) =>
      val lhs = CommutativeApplicative.traverseUnordered(si)(i => Tuple2K(f(i), g(i)))
      val rhs = Tuple2K(CommutativeApplicative.traverseUnordered(si)(f), CommutativeApplicative.traverseUnordered(si)(g))
      lhs should ===(rhs)
    }
  }

  test("traverseUnordered consistent with sequenceUnordered") {
    forAll { (si: Set[Int], f: Int => String) =>
      CommutativeApplicative.traverseUnordered(si)(i => f(i).valid[Int]) should
        === (CommutativeApplicative.sequenceUnordered(si.map(i => f(i).valid[Int])))
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
