package cats
package tests

import cats.data.{Tuple2K, Validated}
import cats.laws.discipline.{TraverseTests, FlatMapTests, SerializableTests, SemigroupalTests}

class MapTests extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Map[Int, ?]]

  checkAll("Map[Int, Int]", SemigroupalTests[Map[Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Map[Int, ?]]", SerializableTests.serializable(Semigroupal[Map[Int, ?]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, ?]]", SerializableTests.serializable(FlatMap[Map[Int, ?]]))

  checkAll("Map[Int, Int] with Option", TraverseTests[Map[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Map[Int, ?]]", SerializableTests.serializable(Traverse[Map[Int, ?]]))

  test("traverseUnordered identity") {
    forAll { (mi: Map[Int, Int], f: (Int, Int) => (String, String)) =>
      CommutativeApplicative[Id].traverseUnorderedMap[Int, String, Int, String](mi)(f) should === (mi.map(f.tupled))
    }
  }

  test("traverseUnordered parallel composition") {
    forAll { (si: Map[Int, Int], f: (Int, Int) => Option[(String, String)], g: (Int, Int) => Option[(String, String)]) =>

      val lhs = CommutativeApplicative[Tuple2K[Option, Option, ?]].traverseUnorderedMap(si)((i, j) => Tuple2K(f(i, j), g(i, j)))
      val rhs = Tuple2K(CommutativeApplicative[Option].traverseUnorderedMap(si)(f), CommutativeApplicative[Option].traverseUnorderedMap(si)(g))
      lhs should ===(rhs)
    }
  }

  test("traverseUnordered consistent with sequenceUnordered") {
    forAll { (mi: Map[Int, Int], f: (Int, Int) => (String, String)) =>
      CommutativeApplicative[Validated[Int, ?]].traverseUnorderedMap(mi)((i,j) => f(i, j).valid[Int]) should
        === (CommutativeApplicative[Validated[Int, ?]].sequenceUnorderedMap(mi.map(i => (f.tupled(i)._1, f.tupled(i).valid[Int]))))
    }
  }

  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      map.show.nonEmpty should === (true)
      map.show.startsWith("Map(") should === (true)
      map.show should === (implicitly[Show[Map[Int, String]]].show(map))
    }
  }
}
