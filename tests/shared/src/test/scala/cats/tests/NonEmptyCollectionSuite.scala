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

import cats.data.NonEmptyCollection
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

abstract class NonEmptyCollectionSuite[U[+_], NE[+_], NEC[x] <: NonEmptyCollection[x, U, NE]](implicit
  arbitraryU: Arbitrary[U[Int]],
  arbitraryNE: Arbitrary[NE[Int]]
) extends CatsSuite {
  protected def toList[A](value: NE[A]): List[A]
  protected def underlyingToList[A](underlying: U[A]): List[A]

  // Necessary because of the non-inheritance-based encoding of some non-empty collections.
  protected def toNonEmptyCollection[A](nea: NE[A]): NEC[A]
  implicit private def convertToNonEmptyCollection[A](nea: NE[A]): NEC[A] = toNonEmptyCollection(nea)

  test("head is consistent with iterator.toList.head") {
    forAll { (is: NE[Int]) =>
      assert(is.head === (is.iterator.toList.head))
    }
  }

  test("tail is consistent with iterator.toList.tail") {
    forAll { (is: NE[Int]) =>
      assert(underlyingToList(is.tail) === (is.iterator.toList.tail))
    }
  }

  test("last is consistent with iterator.toList.last") {
    forAll { (is: NE[Int]) =>
      assert(is.last === (is.iterator.toList.last))
    }
  }

  test("init is consistent with iterator.toList.init") {
    forAll { (is: NE[Int]) =>
      assert(underlyingToList(is.init) === (is.iterator.toList.init))
    }
  }

  test("map is consistent with iterator.toList.map") {
    forAll { (is: NE[Int], f: Int => String) =>
      assert(toList(is.map(f)) === (is.iterator.toList.map(f)))
    }
  }

  test("reverse is consistent with iterator.toList.reverse") {
    forAll { (is: NE[Int]) =>
      assert(toList(is.reverse) === (is.iterator.toList.reverse))
    }
  }

  test("prepend is consistent with iterator.toList.::") {
    forAll { (is: NE[Int], i: Int) =>
      assert(toList(is.prepend(i)) === (i :: is.iterator.toList))
    }
  }

  test("append is consistent with iterator.toList.::") {
    forAll { (is: NE[Int], i: Int) =>
      assert(toList(is.append(i)) === (is.iterator.toList :+ i))
    }
  }

  test("filter is consistent with iterator.toList.filter") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      assert(underlyingToList(is.filter(pred)) === (is.iterator.toList.filter(pred)))
    }
  }

  test("filterNot is consistent with iterator.toList.filterNot") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      assert(underlyingToList(is.filterNot(pred)) === (is.iterator.toList.filterNot(pred)))
    }
  }

  test("collect is consistent with iterator.toList.collect") {
    forAll { (is: NE[Int], pf: PartialFunction[Int, String]) =>
      assert(underlyingToList(is.collect(pf)) === (is.iterator.toList.collect(pf)))
    }
  }

  test("collectFirst is consistent with iterator.toList.collectFirst") {
    forAll { (is: NE[Int], pf: PartialFunction[Int, String]) =>
      assert(is.collectFirst(pf) === is.iterator.toList.collectFirst(pf))
    }
  }

  test("find is consistent with iterator.toList.find") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      assert(is.find(pred) === (is.iterator.toList.find(pred)))
    }
  }

  test("exists is consistent with iterator.toList.exists") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      assert(is.exists(pred) === (is.iterator.toList.exists(pred)))
    }
  }

  test("forall is consistent with iterator.toList.forall") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      assert(is.forall(pred) === (is.iterator.toList.forall(pred)))
    }
  }

  test("foldLeft is consistent with iterator.toList.foldLeft") {
    forAll { (is: NE[Int], b: String, f: (String, Int) => String) =>
      assert(is.foldLeft(b)(f) === (is.iterator.toList.foldLeft(b)(f)))
    }
  }

  test("reduce is consistent with iterator.toList.sum") {
    forAll { (is: NE[Int]) =>
      assert(is.reduce === (is.iterator.toList.sum))
    }
  }

  test("zipWith is consistent with iterator.toList.zip") {
    forAll { (is: NE[Int], other: NE[Int], f: (Int, Int) => String) =>
      assert(toList(is.zipWith(other)(f)) === (is.iterator.toList.zip(other.iterator.toList).map(Function.tupled(f))))
    }
  }

  test("zipWithIndex is consistent with iterator.toList.zipWithIndex") {
    forAll { (is: NE[Int]) =>
      assert(toList(is.zipWithIndex) === (is.iterator.toList.zipWithIndex))
    }
  }

  test("distinct is consistent with iterator.toList.distinct") {
    forAll { (is: NE[Int]) =>
      assert(toList(is.distinct) === (is.iterator.toList.distinct))
    }
  }

  test("sortBy is consistent with iterator.toList.sortBy") {
    forAll { (is: NE[Int], f: Int => String) =>
      assert(toList(is.sortBy(f)) === (is.iterator.toList.sortBy(f)))
    }
  }

  test("sorted is consistent with iterator.toList.sorted") {
    forAll { (is: NE[Int]) =>
      assert(toList(is.sorted) === (is.iterator.toList.sorted))
    }
  }

  test("groupByNem is consistent with iterator.toList.groupBy") {
    forAll { (is: NE[Int], f: Int => String) =>
      assert(
        (is
          .groupByNem(f)
          .toSortedMap
          .map { case (k, v) => (k, toList(v)) }: Map[String, List[Int]]) === (is.iterator.toList.groupBy(f))
      )
    }
  }

  test("grouped is consistent with iterator.toList.grouped") {
    forAll { (is: NE[Int], i: Int) =>
      (i > 0) ==> (is.grouped(i).toList.map(toList) === is.iterator.toList.grouped(i).toList)
    }
  }

  test("toNem is consistent with iterator.toMap") {
    forAll { (is: NE[Int]) =>
      assert((is.zipWithIndex.toNem.toSortedMap: Map[Int, Int]) === (is.zipWithIndex.iterator.toMap))
    }
  }

  test("toNes is consistent with iterator.toSet") {
    forAll { (is: NE[Int]) =>
      assert((is.toNes.toSortedSet: Set[Int]) === (is.iterator.toSet))
    }
  }
}
