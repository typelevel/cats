package cats
package tests

import cats.FunctorOps._

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

  test("widen equals map(identity)") {
    forAll { (i: Int) =>
      val list: List[Some[Int]] = List(Some(i))
      val widened: List[Option[Int]] = list.widen[Option[Int]]
      widened should ===(list.map(identity[Option[Int]]))
      assert(widened eq list)
    }
  }

  test("ifF equals map(if(_) ifTrue else ifFalse) for concrete lists and optoins") {
    val testListFunctor = new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }
    val testOptionFunctor = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }
    testListFunctor.ifF(List(true, false, false, true))(1, 0) should ===(List(1, 0, 0, 1))
    testListFunctor.ifF(List.empty[Boolean])(1, 0) should ===(Nil)
    testOptionFunctor.ifF(Some(true))(1, 0) should ===(Some(1))
    testOptionFunctor.ifF(Some(false))(1, 0) should ===(Some(0))
    testOptionFunctor.ifF(None)(1, 0) should ===(None)
  }
}
