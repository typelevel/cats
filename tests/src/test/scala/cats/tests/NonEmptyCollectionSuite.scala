package cats.tests

import cats.data.NonEmptyCollection
import org.scalacheck.Arbitrary

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
      is.head should ===(is.iterator.toList.head)
    }
  }

  test("tail is consistent with iterator.toList.tail") {
    forAll { (is: NE[Int]) =>
      underlyingToList(is.tail) should ===(is.iterator.toList.tail)
    }
  }

  test("last is consistent with iterator.toList.last") {
    forAll { (is: NE[Int]) =>
      is.last should ===(is.iterator.toList.last)
    }
  }

  test("init is consistent with iterator.toList.init") {
    forAll { (is: NE[Int]) =>
      underlyingToList(is.init) should ===(is.iterator.toList.init)
    }
  }

  test("map is consistent with iterator.toList.map") {
    forAll { (is: NE[Int], f: Int => String) =>
      toList(is.map(f)) should ===(is.iterator.toList.map(f))
    }
  }

  test("reverse is consistent with iterator.toList.reverse") {
    forAll { (is: NE[Int]) =>
      toList(is.reverse) should ===(is.iterator.toList.reverse)
    }
  }

  test("prepend is consistent with iterator.toList.::") {
    forAll { (is: NE[Int], i: Int) =>
      toList(is.prepend(i)) should ===(i :: is.iterator.toList)
    }
  }

  test("append is consistent with iterator.toList.::") {
    forAll { (is: NE[Int], i: Int) =>
      toList(is.append(i)) should ===(is.iterator.toList :+ i)
    }
  }

  test("filter is consistent with iterator.toList.filter") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      underlyingToList(is.filter(pred)) should ===(is.iterator.toList.filter(pred))
    }
  }

  test("filterNot is consistent with iterator.toList.filterNot") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      underlyingToList(is.filterNot(pred)) should ===(is.iterator.toList.filterNot(pred))
    }
  }

  test("collect is consistent with iterator.toList.collect") {
    forAll { (is: NE[Int], pf: PartialFunction[Int, String]) =>
      underlyingToList(is.collect(pf)) should ===(is.iterator.toList.collect(pf))
    }
  }

  test("find is consistent with iterator.toList.find") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      is.find(pred) should ===(is.iterator.toList.find(pred))
    }
  }

  test("exists is consistent with iterator.toList.exists") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      is.exists(pred) should ===(is.iterator.toList.exists(pred))
    }
  }

  test("forall is consistent with iterator.toList.forall") {
    forAll { (is: NE[Int], pred: Int => Boolean) =>
      is.forall(pred) should ===(is.iterator.toList.forall(pred))
    }
  }

  test("foldLeft is consistent with iterator.toList.foldLeft") {
    forAll { (is: NE[Int], b: String, f: (String, Int) => String) =>
      is.foldLeft(b)(f) should ===(is.iterator.toList.foldLeft(b)(f))
    }
  }

  test("reduce is consistent with iterator.toList.sum") {
    forAll { (is: NE[Int]) =>
      is.reduce should ===(is.iterator.toList.sum)
    }
  }

  test("zipWith is consistent with iterator.toList.zip") {
    forAll { (is: NE[Int], other: NE[Int], f: (Int, Int) => String) =>
      toList(is.zipWith(other)(f)) should ===(is.iterator.toList.zip(other.iterator.toList).map(Function.tupled(f)))
    }
  }

  test("zipWithIndex is consistent with iterator.toList.zipWithIndex") {
    forAll { (is: NE[Int]) =>
      toList(is.zipWithIndex) should ===(is.iterator.toList.zipWithIndex)
    }
  }

  test("distinct is consistent with iterator.toList.distinct") {
    forAll { (is: NE[Int]) =>
      toList(is.distinct) should ===(is.iterator.toList.distinct)
    }
  }

  test("sortBy is consistent with iterator.toList.sortBy") {
    forAll { (is: NE[Int], f: Int => String) =>
      toList(is.sortBy(f)) should ===(is.iterator.toList.sortBy(f))
    }
  }

  test("sorted is consistent with iterator.toList.sorted") {
    forAll { (is: NE[Int]) =>
      toList(is.sorted) should ===(is.iterator.toList.sorted)
    }
  }

  test("groupByNem is consistent with iterator.toList.groupBy") {
    forAll { (is: NE[Int], f: Int => String) =>
      is.groupByNem(f).toSortedMap.map { case (k, v) => (k, toList(v)) } should ===(is.iterator.toList.groupBy(f))
    }
  }

  test("toNem is consistent with iterator.toMap") {
    forAll { (is: NE[Int]) =>
      is.zipWithIndex.toNem.toSortedMap should ===(is.zipWithIndex.iterator.toMap)
    }
  }

  test("toNes is consistent with iterator.toSet") {
    forAll { (is: NE[Int]) =>
      is.toNes.toSortedSet should ===(is.iterator.toSet)
    }
  }
}
