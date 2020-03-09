package cats.tests

import cats.Functor
import cats.instances.all._
import cats.syntax.functor._
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.laws.discipline.arbitrary._
import org.scalatest.matchers.must.Matchers._

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
    forAll { (nel: NonEmptyList[Int], o: Option[Int], nem: NonEmptyMap[String, Int]) =>
      val l = nel.toList
      val m = nem.toSortedMap

      Functor[List].unzip(l.map(i => (i, i))) === ((l, l))
      Functor[Option].unzip(o.map(i => (i, i))) === ((o, o))
      Functor[Map[String, *]].unzip(m.map { case (k, v) => (k, (v, v)) }) === ((m, m))

      //postfix test for Cats datatypes
      nel.map(i => (i, i)).unzip === ((nel, nel))
      nem.map(v => (v, v)).unzip === ((nem, nem))
    }

    //empty test for completeness
    val emptyL = List.empty[Int]
    val emptyM = Map.empty[String, Int]

    Functor[List].unzip(List.empty[(Int, Int)]) === ((emptyL, emptyL))
    Functor[Map[String, *]].unzip(Map.empty[String, (Int, Int)]) === ((emptyM, emptyM))
  }

  test("unzip only typechecks for Tuple2") {
    "(NonEmptyList one 1).unzip" mustNot typeCheck
    "(NonEmptyList one ((1, 2))).unzip" must compile
    "(NonEmptyList one ((1, 2, 3))).unzip" mustNot typeCheck
  }

  test("widen equals map(identity)") {
    forAll { (i: Int) =>
      val list: List[Some[Int]] = List(Some(i))
      val widened: List[Option[Int]] = list.widen[Option[Int]]
      widened should ===(list.map(identity[Option[Int]]))
      assert(widened eq list)
    }
  }

  test("ifF equals map(if(_) ifTrue else ifFalse)") {
    forAll { (l: List[Boolean], o: Option[Boolean], m: Map[String, Boolean]) =>
      Functor[List].ifF(l)(1, 0) should ===(l.map(if (_) 1 else 0))
      Functor[Option].ifF(o)(1, 0) should ===(o.map(if (_) 1 else 0))
    }
  }

  test("ifF equals map(if(_) ifTrue else ifFalse) for concrete lists and options") {
    Functor[List].ifF(List(true, false, false, true))(1, 0) should ===(List(1, 0, 0, 1))
    Functor[List].ifF(List.empty[Boolean])(1, 0) should ===(Nil)
    Functor[Option].ifF(Some(true))(1, 0) should ===(Some(1))
    Functor[Option].ifF(Some(false))(1, 0) should ===(Some(0))
    Functor[Option].ifF(None)(1, 0) should ===(None)

  }

}
