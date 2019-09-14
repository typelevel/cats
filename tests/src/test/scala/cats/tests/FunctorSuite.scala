package cats
package tests

class FunctorSuite extends CatsSuite {
  test("void replaces values with unit preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int]) =>
      l.void should ===(List.fill(l.length)(()))
      o.void should ===(if (o.nonEmpty) Some(()) else None)
      m.void should ===(m.keys.map(k => (k, ())).toMap)
    }
  }

  test("as replaces values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      l.as(i) should ===(List.fill(l.length)(i))
      o.as(i) should ===(if (o.nonEmpty) Some(i) else None)
      m.as(i) should ===(m.keys.map(k => (k, i)).toMap)
    }
  }

  test("tupleLeft and tupleRight tuple values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      l.tupleLeft(i) should ===(List.tabulate(l.length)(in => (i, l(in))))
      o.tupleLeft(i) should ===(if (o.nonEmpty) Some((i, o.get)) else None)
      m.tupleLeft(i) should ===(m.map { case (k, v) => (k, (i, v)) }.toMap)
      l.tupleRight(i) should ===(List.tabulate(l.length)(in => (l(in), i)))
      o.tupleRight(i) should ===(if (o.nonEmpty) Some((o.get, i)) else None)
      m.tupleRight(i) should ===(m.map { case (k, v) => (k, (v, i)) }.toMap)
    }
  }

  test("unzip preserves structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int]) =>
      Functor[List].unzip(l.map(i => (i, i))) === ((l, l))
      Functor[Option].unzip(o.map(i => (i, i))) === ((o, o))
      Functor[Map[String, *]].unzip(m.map { case (k, v) => (k, (v, v)) }) === ((m, m))
    }
  }

  test("widen equals map(identity)") {
    forAll { (i: Int) =>
      val list: List[Some[Int]] = List(Some(i))
      val widened: List[Option[Int]] = list.widen[Option[Int]]
      widened should ===(list.map(identity[Option[Int]]))
      assert(widened eq list)
    }
  }
}
