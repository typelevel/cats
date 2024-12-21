/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.Functor
import cats.syntax.functor.*
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.laws.discipline.arbitrary.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class FunctorSuite extends CatsSuite {
  test("void replaces values with unit preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int]) =>
      assert(l.void === (List.fill(l.length)(())))
      assert(o.void === (if (o.nonEmpty) Some(()) else None))
      assert(m.void === (m.keys.map(k => (k, ())).toMap))
    }
  }

  test("as replaces values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      assert(l.as(i) === (List.fill(l.length)(i)))
      assert(o.as(i) === (if (o.nonEmpty) Some(i) else None))
      assert(m.as(i) === (m.keys.map(k => (k, i)).toMap))
    }
  }

  test("tupleLeft and tupleRight tuple values with a constant value preserving structure") {
    forAll { (l: List[Int], o: Option[Int], m: Map[String, Int], i: Int) =>
      assert(l.tupleLeft(i) === (List.tabulate(l.length)(in => (i, l(in)))))
      assert(o.tupleLeft(i) === (if (o.nonEmpty) Some((i, o.get)) else None))
      assert(m.tupleLeft(i) === (m.map { case (k, v) => (k, (i, v)) }.toMap))
      assert(l.tupleRight(i) === (List.tabulate(l.length)(in => (l(in), i))))
      assert(o.tupleRight(i) === (if (o.nonEmpty) Some((o.get, i)) else None))
      assert(m.tupleRight(i) === (m.map { case (k, v) => (k, (v, i)) }.toMap))
    }
  }

  test("unzip preserves structure") {
    forAll { (nel: NonEmptyList[Int], o: Option[Int], nem: NonEmptyMap[String, Int]) =>
      val l = nel.toList
      val m = nem.toSortedMap

      assert(Functor[List].unzip(l.map(i => (i, i))) === ((l, l)))
      assert(Functor[Option].unzip(o.map(i => (i, i))) === ((o, o)))
      assert(Functor[Map[String, *]].unzip(m.map { case (k, v) => (k, (v, v)) }) === ((m, m)))

      // postfix test for Cats datatypes
      assert(nel.map(i => (i, i)).unzip === ((nel, nel)))
      assert(nem.map(v => (v, v)).unzip === ((nem, nem)))
    }

    // empty test for completeness
    val emptyL = List.empty[Int]
    val emptyM = Map.empty[String, Int]

    assert(Functor[List].unzip(List.empty[(Int, Int)]) === ((emptyL, emptyL)))
    assert(Functor[Map[String, *]].unzip(Map.empty[String, (Int, Int)]) === ((emptyM, emptyM)))
  }

  test("_1F, _2F and swapF form correct lists for concrete list of tuples") {
    forAll { (l: List[(Int, Int)]) =>
      val (l1, l2) = l.unzip
      assertEquals(l._1F, l1)
      assertEquals(l._2F, l2)
      assertEquals(l.swapF, l2.zip(l1))
    }
  }

  test("widen equals map(identity)") {
    forAll { (i: Int) =>
      val list: List[Some[Int]] = List(Some(i))
      val widened: List[Option[Int]] = list.widen[Option[Int]]
      assert(widened === (list.map(identity[Option[Int]])))
      assert(widened eq list)
    }
  }

  test("ifF equals map(if(_) ifTrue else ifFalse)") {
    forAll { (l: List[Boolean], o: Option[Boolean], m: Map[String, Boolean]) =>
      assert(Functor[List].ifF(l)(1, 0) === (l.map(if (_) 1 else 0)))
      assert(Functor[Option].ifF(o)(1, 0) === (o.map(if (_) 1 else 0)))
    }
  }

  test("ifF equals map(if(_) ifTrue else ifFalse) for concrete lists and options") {
    assert(Functor[List].ifF(List(true, false, false, true))(1, 0) === (List(1, 0, 0, 1)))
    assert(Functor[List].ifF(List.empty[Boolean])(1, 0) === Nil)
    assert(Functor[Option].ifF(Some(true))(1, 0) === (Some(1)))
    assert(Functor[Option].ifF(Some(false))(1, 0) === (Some(0)))
    assert(Functor[Option].ifF(None)(1, 0) === None)

  }

}
