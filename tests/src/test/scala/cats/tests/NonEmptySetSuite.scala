/*
 * Copyright (c) 2018 Luka Jacobowitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats
package tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.data.NonEmptySortedSet
import cats.kernel.laws.discipline.{SemilatticeTests, EqTests}

import scala.collection.immutable.SortedSet

class NonEmptySortedSetSuite extends CatsSuite {

  checkAll("NonEmptySortedSet[Int]", SemigroupKTests[NonEmptySortedSet].semigroupK[Int])
  checkAll("NonEmptySortedSet[Int]", ReducibleTests[NonEmptySortedSet].reducible[Option, Int, Int])
  checkAll("NonEmptySortedSet[String]", SemilatticeTests[NonEmptySortedSet[String]].band)
  checkAll("NonEmptySortedSet[String]", EqTests[NonEmptySortedSet[String]].eqv)

  test("First element is always the smallest") {
    forAll { (nes: NonEmptySortedSet[Int]) =>
      nes.forall { v => Order[Int].lteqv(nes.head, v) } should === (true)

    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nes: NonEmptySortedSet[Int]) =>
      nes.show.nonEmpty should === (true)
      nes.show.startsWith("NonEmptySortedSet(") should === (true)
      nes.show should === (implicitly[Show[NonEmptySortedSet[Int]]].show(nes))
      nes.show.contains(nes.head.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptySet = NonEmptySortedSet("Test", SortedSet.empty[String])
    nonEmptySet.show should === ("NonEmptySortedSet(Test)")
  }

  test("Creating NonEmptySortedSet + toSet is identity") {
    forAll { (i: Int, tail: SortedSet[Int]) =>
      val set = tail + i
      val nonEmptySet = NonEmptySortedSet(i, tail)
      set should === (nonEmptySet.toSortedSet)
    }
  }

  test("NonEmptySortedSet#filter is consistent with Set#filter") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.filter(p) should === (set.filter(p))
    }
  }

  test("NonEmptySortedSet#filterNot is consistent with Set#filterNot") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.filterNot(p) should === (set.filterNot(p))
    }
  }

  test("NonEmptySortedSet#collect is consistent with Set#collect") {
    forAll { (nes: NonEmptySortedSet[Int], pf: PartialFunction[Int, String]) =>
      val set = nes.toSortedSet
      nes.collect(pf) should === (set.collect(pf))
    }
  }

  test("NonEmptySortedSet#find is consistent with Set#find") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.find(p) should === (set.find(p))
    }
  }

  test("NonEmptySortedSet#exists is consistent with Set#exists") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.exists(p) should === (set.exists(p))
    }
  }

  test("NonEmptySortedSet#forall is consistent with Set#forall") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.forall(p) should === (set.forall(p))
    }
  }

  test("NonEmptySortedSet#map is consistent with Set#map") {
    forAll { (nes: NonEmptySortedSet[Int], p: Int => String) =>
      val set = nes.toSortedSet
      nes.map(p).toSortedSet should === (set.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nes: NonEmptySortedSet[Int], f: (Int, Int) => Int) =>
      nes.reduceLeft(f) should === (nes.tail.foldLeft(nes.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nes: NonEmptySortedSet[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nes.reduceRight(f).value
      val last = nes.last
      val rev = nes - last
      val expected = rev.foldRight(last)((a, b) => f(a, Now(b)).value)
      got should === (expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nes: NonEmptySortedSet[Int]) =>
      nes.reduce should === (nes.fold)
    }
  }


  test("reduce consistent with reduceK") {
    forAll { (nes: NonEmptySortedSet[Option[Int]]) =>
      nes.reduce(SemigroupK[Option].algebra[Int]) should === (nes.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nes: NonEmptySortedSet[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nes.tail.foldLeft(Option(f(nes.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nes.reduceLeftToOption(f)(g) should === (expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nes: NonEmptySortedSet[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nes.reduceRightToOption(f)(g).value
      val last = nes.last
      val rev = nes - last
      val expected = rev.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      got should === (expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nes: NonEmptySortedSet[Int], f: Int => Option[Int]) =>
      val got = nes.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nes.head).flatMap { hd =>
        nes.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      got should === (expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nes: NonEmptySortedSet[Int], f: Int => Option[Int]) =>
      nes.reduceMapM(f) should === (nes.foldMapM(f))
    }
  }

  test("fromSet round trip") {
    forAll { l: SortedSet[Int] =>
      NonEmptySortedSet.fromSet(l).map(_.toSortedSet).getOrElse(SortedSet.empty[Int]) should === (l)
    }

    forAll { nes: NonEmptySortedSet[Int] =>
      NonEmptySortedSet.fromSet(nes.toSortedSet) should === (Some(nes))
    }
  }

  test("fromSetUnsafe/fromSet consistency") {
    forAll { nes: NonEmptySortedSet[Int] =>
      NonEmptySortedSet.fromSet(nes.toSortedSet) should === (Some(NonEmptySortedSet.fromSetUnsafe(nes.toSortedSet)))
    }
  }

  test("fromSetUnsafe empty set") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptySortedSet.fromSetUnsafe(SortedSet.empty[Int])
    }
  }

  test("+ consistent with Set") {
    forAll { (nes: NonEmptySortedSet[Int], i: Int) =>
      (nes add i).toSortedSet should === (nes.toSortedSet + i)
    }
  }

  test("NonEmptySortedSet#zipWithIndex is consistent with Set#zipWithIndex") {
    forAll { nes: NonEmptySortedSet[Int] =>
      nes.zipWithIndex.toSortedSet should === (nes.toSortedSet.zipWithIndex)
    }
  }

  test("NonEmptySortedSet#length is consistent with Set#size") {
    forAll { nes: NonEmptySortedSet[Int] =>
      nes.length should === (nes.toSortedSet.size)
    }
  }

  test("NonEmptySortedSet#concat is consistent with Set#++") {
    forAll { (nes: NonEmptySortedSet[Int], l: SortedSet[Int], n: Int) =>
      nes.union(NonEmptySortedSet(n, l)).toSortedSet should === (nes.toSortedSet ++ (l + n))
    }
  }

  test("NonEmptySortedSet#zipWith is consistent with Set#zip and then Set#map") {
    forAll { (a: NonEmptySortedSet[Int], b: NonEmptySortedSet[Int], f: (Int, Int) => Int) =>
      a.zipWith(b)(f).toSortedSet should ===(a.toSortedSet.zip(b.toSortedSet).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptySortedSet.of is consistent with removal") {
    forAll { (is: SortedSet[Int], i: Int) =>
      NonEmptySortedSet.of(i, is.toList: _*) - i should ===(is - i)
    }
  }

  test("NonEmptySortedSet#groupBy is consistent with Set#groupBy") {
    forAll { (nes: NonEmptySortedSet[Int], f: Int => Int) =>
      nes.groupBy(f).map(_.toSortedSet).toSortedMap should === (nes.toSortedSet.groupBy(f))
    }
  }
}
