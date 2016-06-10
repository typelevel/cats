package cats
package tests

class FunctorTest extends CatsSuite {
  test("void replaces values with unit preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int]) =>
      l.void should === (List.fill(l.length)(()))
      o.void should === (if (o.nonEmpty) Some(()) else None)
      m.void should === (m.keys.map(k => (k, ())).toMap)
    }
  }

  test("as replaces values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      l.as(i) should === (List.fill(l.length)(i))
      o.as(i) should === (if (o.nonEmpty) Some(i) else None)
      m.as(i) should === (m.keys.map(k => (k, i)).toMap)
    }
  }

  test("widen equals map(identity)") {
    forAll { (i: Int) =>
      val list: List[Some[Int]] = List(Some(i))
      val widened: List[Option[Int]] = list.widen[Option[Int]]
      widened should === (list.map(identity[Option[Int]]))
      assert(widened eq list)
    }
  }
}
