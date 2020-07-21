package cats.tests

import cats.{Eval, Now, Reducible, SemigroupK, Show}
import cats.data.NonEmptySet
import cats.kernel.{Eq, Order, PartialOrder, Semilattice}
import cats.kernel.laws.discipline.{EqTests, HashTests, OrderTests, SemilatticeTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.syntax.show._
import scala.collection.immutable.SortedSet

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
      nes.forall { v =>
        Order[Int].lteqv(nes.head, v)
      } should ===(true)

    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nes: NonEmptySet[Int]) =>
      nes.show.nonEmpty should ===(true)
      nes.show.startsWith("NonEmptySortedSet(") should ===(true)
      nes.show should ===(implicitly[Show[NonEmptySet[Int]]].show(nes))
      nes.show.contains(nes.head.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptySet = NonEmptySet("Test", SortedSet.empty[String])
    nonEmptySet.show should ===("NonEmptySortedSet(Test)")
  }

  test("Creating NonEmptySet + toSet is identity") {
    forAll { (i: Int, tail: SortedSet[Int]) =>
      val set = tail + i
      val nonEmptySet = NonEmptySet(i, tail)
      set should ===(nonEmptySet.toSortedSet)
    }
  }

  test("NonEmptySet#filter is consistent with Set#filter") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.filter(p) should ===(set.filter(p))
    }
  }

  test("NonEmptySet#filterNot is consistent with Set#filterNot") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.filterNot(p) should ===(set.filterNot(p))
    }
  }

  test("NonEmptySet#collect is consistent with Set#collect") {
    forAll { (nes: NonEmptySet[Int], pf: PartialFunction[Int, String]) =>
      val set = nes.toSortedSet
      nes.collect(pf) should ===(set.collect(pf))
    }
  }

  test("NonEmptySet#find is consistent with Set#find") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.find(p) should ===(set.find(p))
    }
  }

  test("NonEmptySet#exists is consistent with Set#exists") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.exists(p) should ===(set.exists(p))
    }
  }

  test("NonEmptySet#forall is consistent with Set#forall") {
    forAll { (nes: NonEmptySet[Int], p: Int => Boolean) =>
      val set = nes.toSortedSet
      nes.forall(p) should ===(set.forall(p))
    }
  }

  test("NonEmptySet#map is consistent with Set#map") {
    forAll { (nes: NonEmptySet[Int], p: Int => String) =>
      val set = nes.toSortedSet
      nes.map(p).toSortedSet should ===(set.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nes: NonEmptySet[Int], f: (Int, Int) => Int) =>
      nes.reduceLeft(f) should ===(nes.tail.foldLeft(nes.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nes: NonEmptySet[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nes.reduceRight(f).value
      val last = nes.last
      val rev = nes - last
      val expected = rev.foldRight(last)((a, b) => f(a, Now(b)).value)
      got should ===(expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nes: NonEmptySet[Int]) =>
      nes.reduce should ===(nes.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nes: NonEmptySet[Option[Int]]) =>
      nes.reduce(SemigroupK[Option].algebra[Int]) should ===(nes.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nes: NonEmptySet[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nes.tail.foldLeft(Option(f(nes.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nes.reduceLeftToOption(f)(g) should ===(expected)
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
      got should ===(expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nes: NonEmptySet[Int], f: Int => Option[Int]) =>
      val got = nes.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nes.head).flatMap { hd =>
        nes.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      got should ===(expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nes: NonEmptySet[Int], f: Int => Option[Int]) =>
      nes.reduceMapM(f) should ===(nes.foldMapM(f))
    }
  }

  test("fromSet round trip") {
    forAll { (l: SortedSet[Int]) =>
      NonEmptySet.fromSet(l).map(_.toSortedSet).getOrElse(SortedSet.empty[Int]) should ===(l)
    }

    forAll { (nes: NonEmptySet[Int]) =>
      NonEmptySet.fromSet(nes.toSortedSet) should ===(Some(nes))
    }
  }

  test("fromSetUnsafe/fromSet consistency") {
    forAll { (nes: NonEmptySet[Int]) =>
      NonEmptySet.fromSet(nes.toSortedSet) should ===(Some(NonEmptySet.fromSetUnsafe(nes.toSortedSet)))
    }
  }

  test("fromSetUnsafe empty set") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptySet.fromSetUnsafe(SortedSet.empty[Int])
    }
  }

  test("+ consistent with Set") {
    forAll { (nes: NonEmptySet[Int], i: Int) =>
      nes.add(i).toSortedSet should ===(nes.toSortedSet + i)
    }
  }

  test("NonEmptySet#zipWithIndex is consistent with Set#zipWithIndex") {
    forAll { (nes: NonEmptySet[Int]) =>
      nes.zipWithIndex.toSortedSet should ===(nes.toSortedSet.zipWithIndex)
    }
  }

  test("NonEmptySet#length is consistent with Set#size") {
    forAll { (nes: NonEmptySet[Int]) =>
      nes.length should ===(nes.toSortedSet.size)
    }
  }

  test("NonEmptySet#concat is consistent with Set#++") {
    forAll { (nes: NonEmptySet[Int], l: SortedSet[Int], n: Int) =>
      nes.union(NonEmptySet(n, l)).toSortedSet should ===(nes.toSortedSet ++ (l + n))
    }
  }

  test("NonEmptySet#zipWith is consistent with Set#zip and then Set#map") {
    forAll { (a: NonEmptySet[Int], b: NonEmptySet[Int], f: (Int, Int) => Int) =>
      a.zipWith(b)(f).toSortedSet should ===(a.toSortedSet.zip(b.toSortedSet).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptySet.of is consistent with removal") {
    forAll { (is: SortedSet[Int], i: Int) =>
      NonEmptySet.of(i, is.toList: _*) - i should ===(is - i)
    }
  }

  test("NonEmptySet#groupBy is consistent with Set#groupBy") {
    forAll { (nes: NonEmptySet[Int], f: Int => Int) =>
      nes.groupBy(f).map(_.toSortedSet).toSortedMap should ===(nes.toSortedSet.groupBy(f))
    }
  }
}
