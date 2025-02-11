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

import cats.{Align, Eval, Foldable, Now, Semigroup, SemigroupK, Show}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.kernel.laws.discipline.{SerializableTests => _, *}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.bifunctor.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.reducible.*
import cats.syntax.show.*
import scala.collection.immutable.SortedMap
import org.scalacheck.Prop.*

class NonEmptyMapSuite extends CatsSuite {

  checkAll("NonEmptyMap[String, Int]", SemigroupKTests[NonEmptyMap[String, *]].semigroupK[Int])
  checkAll(
    "NonEmptyMap[String, Int]",
    NonEmptyTraverseTests[NonEmptyMap[String, *]].nonEmptyTraverse[Option, Int, Int, Double, Int, Option, Option]
  )
  checkAll("NonEmptyMap[String, Int]", SemigroupTests[NonEmptyMap[String, Int]].semigroup)
  checkAll("NonEmptyMap[String, Int]", EqTests[NonEmptyMap[String, Int]].eqv)
  checkAll("NonEmptyMap[String, Int]", HashTests[NonEmptyMap[String, Int]].hash)

  checkAll("NonEmptyMap[String, Int]", AlignTests[NonEmptyMap[String, *]].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyMap]", SerializableTests.serializable(Align[NonEmptyMap[String, *]]))

  checkAll("NonEmptyMap[Int, *]", ShortCircuitingTests[NonEmptyMap[Int, *]].nonEmptyTraverse[Int])

  test("Show is not empty and is formatted as expected") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(nem.show.nonEmpty === true)
      assert(nem.show.startsWith("NonEmptySortedMap(") === true)
      assert(nem.show === (implicitly[Show[NonEmptyMap[String, Int]]].show(nem)))
      assert(nem.show.contains(nem.head._2.show) === true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyMap = NonEmptyMap.one("Key", "Test")
    assert(nonEmptyMap.show === "NonEmptySortedMap(Key -> Test)")
  }

  test("NonEmptyMap#filter is consistent with Map#filter") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      assert(nem.filter(p) === (map.filter(t => p(t._2))))
    }
  }

  test("NonEmptyMap#filterNot is consistent with Map#filterNot") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      assert(nem.filterNot(p) === (map.filterNot(t => p(t._2))))
    }
  }

  test("NonEmptyMap#find is consistent with Map#find") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      assert(nem.find(p) === (map.find(p)))
    }
  }

  test("NonEmptyMap#exists is consistent with Map#exists") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      assert(nem.exists(p) === (map.exists(p)))
    }
  }

  test("NonEmptyMap#forall is consistent with Map#forall") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      assert(nem.forall(p) === (map.forall(p)))
    }
  }

  test("NonEmptyMap#map is consistent with Map#map") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => String) =>
      val map = nem.toSortedMap
      assert(nem.map(p).toSortedMap === (map.fmap(p)))
    }
  }

  test("NonEmptyMap#mapKeys is consistent with Map#map") {
    forAll { (nem: NonEmptyMap[String, Int], p: String => Int) =>
      val map = nem.toSortedMap
      assert(nem.mapKeys(p).toSortedMap === map.map(_.leftMap(p)))
    }
  }

  test("NonEmptyMap#mapBoth is consistent with Map#map") {
    forAll { (nem: NonEmptyMap[String, Int], p: (String, Int) => (Int, String)) =>
      val map = nem.toSortedMap
      assert(nem.mapBoth(p).toSortedMap === map.map(Function.tupled(p)))
    }
  }

  test("NonEmptyMap#transform is consistent with Map#transform") {
    forAll { (nem: NonEmptyMap[String, Int], p: (String, Int) => String) =>
      val map = nem.toSortedMap
      assert(nem.transform(p).toSortedMap === map.transform(p))
    }
  }

  test("lookup is consistent with contains") {
    forAll { (nem: NonEmptyMap[String, Int], key: String) =>
      assert(nem(key).isDefined === (nem.contains(key)))
    }
  }

  test("keys.contains is consistent with contains") {
    forAll { (nem: NonEmptyMap[String, Int], key: String) =>
      assert(nem(key).isDefined === (nem.keys.contains(key)))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nem: NonEmptyMap[String, Int], f: (Int, Int) => Int) =>
      assert(nem.reduceLeft(f) === (Foldable[SortedMap[String, *]].foldLeft(nem.tail, nem.head._2)(f)))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nem: NonEmptyMap[String, Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nem.reduceRight(f).value
      val last = nem.last
      val rev = nem - last._1
      val expected = Foldable[SortedMap[String, *]]
        .foldRight(rev, Now(last._2))((a, b) => f(a, b))
        .value
      assert(got === expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(nem.reduce === (nem.fold))
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nem: NonEmptyMap[String, Option[Int]]) =>
      assert(nem.reduce(SemigroupK[Option].algebra[Int]) === (nem.reduceK))
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nem.tail.foldLeft(Option(f(nem.head._2))) { (opt, i) =>
        opt.map(s => g(s, i._2))
      }
      assert(nem.reduceLeftToOption(f)(g) === expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nem.reduceRightToOption(f)(g).value
      val last = nem.last
      val rev = nem - last._1
      val expected = rev.foldRight(Option(f(last._2))) { (i, opt) =>
        opt.map(s => g(i._2, Now(s)).value)
      }
      assert(got === expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      val got = nem.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nem.head._2).flatMap { hd =>
        nem.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      assert(got === expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      assert(nem.reduceMapM(f) === (nem.foldMapM(f)))
    }
  }

  test("fromMap round trip") {
    forAll { (l: SortedMap[String, Int]) =>
      assert(NonEmptyMap.fromMap(l).map(_.toSortedMap).getOrElse(SortedMap.empty[String, Int]) === l)
    }

    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(NonEmptyMap.fromMap(nem.toSortedMap) === (Some(nem)))
    }
  }

  test("fromMapUnsafe/fromMap consistency") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(NonEmptyMap.fromMap(nem.toSortedMap) === (Some(NonEmptyMap.fromMapUnsafe(nem.toSortedMap))))
    }
  }

  test("fromMapUnsafe empty map") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyMap.fromMapUnsafe(SortedMap.empty[String, Int])
    }
  }

  test("+ consistent with Map") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      assert(nem.add(i).toSortedMap === (nem.toSortedMap + i))
    }
  }

  test("NonEmptyMap#size and length is consistent with Map#size") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(nem.size === (nem.toSortedMap.size.toLong))
      assert(nem.length === (nem.toSortedMap.size))
    }
  }

  test("NonEmptyMap#toNonEmptyList is consistent with Map#toList and creating NonEmptyList from it") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      assert(nem.toNel === (NonEmptyList.fromListUnsafe(nem.toSortedMap.toList)))
    }
  }

  test("NonEmptyMap#updateWith identity should be a no-op") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      assert(nem.add(i) === (nem.add(i).updateWith(i._1)(identity)))
    }
  }

  test("NonEmptyMap#updateWith on existing value should behave like Option#map on the same value") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      assert(nem.add(i).lookup(i._1).map(_ + 1) === (nem.add(i).updateWith(i._1)(_ + 1).lookup(i._1)))
    }
  }

  test("NonEmptyMap#updateWith should not act when key is missing") {
    val single = NonEmptyMap[String, Int](("here", 1), SortedMap())
    assert(single.lookup("notHere") === (single.updateWith("notHere")(_ => 1).lookup("notHere")))
  }

  test("combine should be consistent with SortedMap") {
    forAll { (nem1: NonEmptyMap[Int, Int], nem2: NonEmptyMap[Int, Int]) =>
      val lhs = Semigroup.combine(nem1, nem2).toSortedMap
      val rhs = Semigroup.combine(nem1.toSortedMap, nem2.toSortedMap)
      assert(lhs === rhs)
    }
  }

  test("Semigroup[NonEmptyMap[K, V]] should require Semigroup[V]") {
    assert(compileErrors("Semigroup[NonEmptyMap[Int, Char]]").nonEmpty)
  }
}
