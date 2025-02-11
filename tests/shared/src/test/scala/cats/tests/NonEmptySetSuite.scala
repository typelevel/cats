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

import cats.{Eval, Now, Reducible, SemigroupK, Show}
import cats.data.NonEmptySet
import cats.kernel.{Eq, Order, PartialOrder, Semilattice}
import cats.kernel.laws.discipline.{EqTests, HashTests, OrderTests, SemilatticeTests}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.syntax.show.*
import scala.collection.immutable.SortedSet
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class NonEmptySetSuite extends CatsSuite {

  checkAll("NonEmptySet[Int]", SemigroupKTests[NonEmptySet].semigroupK[Int])
  checkAll("SemigroupK[NonEmptySet[A]]", SerializableTests.serializable(SemigroupK[NonEmptySet]))

  checkAll("NonEmptySet[Int]", ReducibleTests[NonEmptySet].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptySet]", SerializableTests.serializable(Reducible[NonEmptySet]))

  checkAll("NonEmptySet[String]", SemilatticeTests[NonEmptySet[String]].band)
  checkAll("Semilattice[NonEmptySet]", SerializableTests.serializable(Semilattice[NonEmptySet[String]]))

  checkAll("NonEmptySet[String]", EqTests[NonEmptySet[String]].eqv)
  checkAll("NonEmptySet[String]", HashTests[NonEmptySet[String]].hash)

  {
    implicit val A: Order[ListWrapper[Int]] = ListWrapper.order[Int]
    checkAll("Eq[NonEmptySet[ListWrapper[Int]]]", SerializableTests.serializable(Eq[NonEmptySet[ListWrapper[Int]]]))

    checkAll("NonEmptySet[ListWrapper[Int]]", OrderTests[NonEmptySet[ListWrapper[Int]]].order)
    checkAll("Order[NonEmptySet[ListWrapper[Int]]]",
             SerializableTests.serializable(Order[NonEmptySet[ListWrapper[Int]]])
    )

    Eq[NonEmptySet[ListWrapper[Int]]]
    PartialOrder[NonEmptySet[ListWrapper[Int]]]
  }

  checkAll("NonEmptySet[Int]", ShortCircuitingTests[NonEmptySet].foldable[Int])

  test("First element is always the smallest") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(nes.forall { v =>
        Order[Int].lteqv(nes.head, v)
      })
    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(nes.show.nonEmpty)
      assert(nes.show.startsWith("NonEmptySortedSet("))
      assert(nes.show === (implicitly[Show[NonEmptySet[Int]]].show(nes)))
      assert(nes.show.contains(nes.head.show))
    }
  }

  test("Show is formatted correctly") {
    val nonEmptySet = NonEmptySet("Test", SortedSet.empty[String])
    assert(nonEmptySet.show === "NonEmptySortedSet(Test)")
  }

  test("Creating NonEmptySet + toSet is identity") {
    forAll { (i: Int, tail: SortedSet[Int]) =>
      val set = tail + i
      val nonEmptySet = NonEmptySet(i, tail)
      assert(set === (nonEmptySet.toSortedSet))
    }
  }

  test("NonEmptySet#filter is consistent with Set#filter") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      assert(nes.filter(p) === (set.filter(p)))
    }
  }

  test("NonEmptySet#filterNot is consistent with Set#filterNot") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      assert(nes.filterNot(p) === (set.filterNot(p)))
    }
  }

  test("NonEmptySet#collect is consistent with Set#collect") {
    forAll { (nes: NonEmptySet[Int], pf: PartialFunction[Int, String]) =>
      val set = nes.toSortedSet
      assert(nes.collect(pf) === (set.collect(pf)))
    }
  }

  test("NonEmptySet#find is consistent with Set#find") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      assert(nes.find(p) === (set.find(p)))
    }
  }

  test("NonEmptySet#exists is consistent with Set#exists") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      assert(nes.exists(p) === (set.exists(p)))
    }
  }

  test("NonEmptySet#forall is consistent with Set#forall") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      assert(nes.forall(p) === (set.forall(p)))
    }
  }

  test("NonEmptySet#map is consistent with Set#map") {
    forAll { (nes: NonEmptySet[Int], p: Int => String) =>
      val set = nes.toSortedSet
      assert(nes.map(p).toSortedSet === (set.map(p)))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nes: NonEmptySet[Int], f: (Int, Int) => Int) =>
      assert(nes.reduceLeft(f) === (nes.tail.foldLeft(nes.head)(f)))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nes: NonEmptySet[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nes.reduceRight(f).value
      val last = nes.last
      val rev = nes - last
      val expected = rev.foldRight(last)((a, b) => f(a, Now(b)).value)
      assert(got === expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(nes.reduce === (nes.fold))
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nes: NonEmptySet[Option[Int]]) =>
      assert(nes.reduce(SemigroupK[Option].algebra[Int]) === (nes.reduceK))
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nes: NonEmptySet[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nes.tail.foldLeft(Option(f(nes.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      assert(nes.reduceLeftToOption(f)(g) === expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nes: NonEmptySet[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nes.reduceRightToOption(f)(g).value
      val last = nes.last
      val rev = nes - last
      val expected = rev.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      assert(got === expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nes: NonEmptySet[Int], f: Int => Option[Int]) =>
      val got = nes.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nes.head).flatMap { hd =>
        nes.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      assert(got === expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nes: NonEmptySet[Int], f: Int => Option[Int]) =>
      assert(nes.reduceMapM(f) === (nes.foldMapM(f)))
    }
  }

  test("fromSet round trip") {
    forAll { (l: SortedSet[Int]) =>
      assert(NonEmptySet.fromSet(l).map(_.toSortedSet).getOrElse(SortedSet.empty[Int]) === l)
    }

    forAll { (nes: NonEmptySet[Int]) =>
      assert(NonEmptySet.fromSet(nes.toSortedSet) === (Some(nes)))
    }
  }

  test("fromSetUnsafe/fromSet consistency") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(NonEmptySet.fromSet(nes.toSortedSet) === (Some(NonEmptySet.fromSetUnsafe(nes.toSortedSet))))
    }
  }

  test("fromSetUnsafe empty set") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptySet.fromSetUnsafe(SortedSet.empty[Int])
    }
  }

  test("+ consistent with Set") {
    forAll { (nes: NonEmptySet[Int], i: Int) =>
      assert(nes.add(i).toSortedSet === (nes.toSortedSet + i))
    }
  }

  test("NonEmptySet#zipWithIndex is consistent with Set#zipWithIndex") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(nes.toSortedSet.zipWithIndex === nes.zipWithIndex.toSortedSet)
    }
  }

  test("NonEmptySet#length is consistent with Set#size") {
    forAll { (nes: NonEmptySet[Int]) =>
      assert(nes.length === (nes.toSortedSet.size))
    }
  }

  test("NonEmptySet#concat is consistent with Set#++") {
    forAll { (nes: NonEmptySet[Int], l: SortedSet[Int], n: Int) =>
      assert(nes.union(NonEmptySet(n, l)).toSortedSet === (nes.toSortedSet ++ (l + n)))
    }
  }

  test("NonEmptySet#zipWith is consistent with Set#zip and then Set#map") {
    forAll { (a: NonEmptySet[Int], b: NonEmptySet[Int], f: (Int, Int) => Int) =>
      assert(a.zipWith(b)(f).toSortedSet === a.toSortedSet.zip(b.toSortedSet).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptySet.of is consistent with removal") {
    forAll { (is: SortedSet[Int], i: Int) =>
      assert(NonEmptySet.of(i, is.toList: _*) - i === (is - i))
    }
  }

  test("NonEmptySet#groupBy is consistent with Set#groupBy") {
    forAll { (nes: NonEmptySet[Int], f: Int => Int) =>
      assert((nes.groupBy(f).map(_.toSortedSet).toSortedMap: Map[Int, SortedSet[Int]]) === (nes.toSortedSet.groupBy(f)))
    }
  }
}
