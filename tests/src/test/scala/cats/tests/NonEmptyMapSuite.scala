package cats.tests

import cats.{Align, Eval, Foldable, Now, SemigroupK, Show}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.kernel.laws.discipline.{SerializableTests => _, _}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.reducible._
import scala.collection.immutable.SortedMap

class NonEmptyMapSuite extends CatsSuite {

  checkAll("NonEmptyMap[String, Int]", SemigroupKTests[NonEmptyMap[String, *]].semigroupK[Int])
  checkAll(
    "NonEmptyMap[String, Int]",
    NonEmptyTraverseTests[NonEmptyMap[String, *]].nonEmptyTraverse[Option, Int, Int, Double, Int, Option, Option]
  )
  checkAll("NonEmptyMap[String, Int]", BandTests[NonEmptyMap[String, Int]].band)
  checkAll("NonEmptyMap[String, Int]", EqTests[NonEmptyMap[String, Int]].eqv)
  checkAll("NonEmptyMap[String, Int]", HashTests[NonEmptyMap[String, Int]].hash)

  checkAll("NonEmptyMap[String, Int]", AlignTests[NonEmptyMap[String, *]].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyMap]", SerializableTests.serializable(Align[NonEmptyMap[String, *]]))

  checkAll("NonEmptyMap[Int, *]", ShortCircuitingTests[NonEmptyMap[Int, *]].nonEmptyTraverse[Int])

  test("Show is not empty and is formatted as expected") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.show.nonEmpty should ===(true)
      nem.show.startsWith("NonEmptySortedMap(") should ===(true)
      nem.show should ===(implicitly[Show[NonEmptyMap[String, Int]]].show(nem))
      nem.show.contains(nem.head._2.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyMap = NonEmptyMap.one("Key", "Test")
    nonEmptyMap.show should ===("NonEmptySortedMap(Key -> Test)")
  }

  test("NonEmptyMap#filter is consistent with Map#filter") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      nem.filter(p) should ===(map.filter(t => p(t._2)))
    }
  }

  test("NonEmptyMap#filterNot is consistent with Map#filterNot") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      nem.filterNot(p) should ===(map.filterNot(t => p(t._2)))
    }
  }

  test("NonEmptyMap#find is consistent with Map#find") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      nem.find(p) should ===(map.find(p))
    }
  }

  test("NonEmptyMap#exists is consistent with Map#exists") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      nem.exists(p) should ===(map.exists(p))
    }
  }

  test("NonEmptyMap#forall is consistent with Map#forall") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toSortedMap
      nem.forall(p) should ===(map.forall(p))
    }
  }

  test("NonEmptyMap#map is consistent with Map#map") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => String) =>
      val map = nem.toSortedMap
      nem.map(p).toSortedMap should ===(map.fmap(p))
    }
  }

  test("lookup is consistent with contains") {
    forAll { (nem: NonEmptyMap[String, Int], key: String) =>
      nem(key).isDefined should ===(nem.contains(key))
    }
  }

  test("keys.contains is consistent with contains") {
    forAll { (nem: NonEmptyMap[String, Int], key: String) =>
      nem(key).isDefined should ===(nem.keys.contains(key))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nem: NonEmptyMap[String, Int], f: (Int, Int) => Int) =>
      nem.reduceLeft(f) should ===(Foldable[SortedMap[String, *]].foldLeft(nem.tail, nem.head._2)(f))
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
      got should ===(expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.reduce should ===(nem.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nem: NonEmptyMap[String, Option[Int]]) =>
      nem.reduce(SemigroupK[Option].algebra[Int]) should ===(nem.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nem.tail.foldLeft(Option(f(nem.head._2))) { (opt, i) =>
        opt.map(s => g(s, i._2))
      }
      nem.reduceLeftToOption(f)(g) should ===(expected)
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
      got should ===(expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      val got = nem.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nem.head._2).flatMap { hd =>
        nem.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      got should ===(expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      nem.reduceMapM(f) should ===(nem.foldMapM(f))
    }
  }

  test("fromMap round trip") {
    forAll { (l: SortedMap[String, Int]) =>
      NonEmptyMap.fromMap(l).map(_.toSortedMap).getOrElse(SortedMap.empty[String, Int]) should ===(l)
    }

    forAll { (nem: NonEmptyMap[String, Int]) =>
      NonEmptyMap.fromMap(nem.toSortedMap) should ===(Some(nem))
    }
  }

  test("fromMapUnsafe/fromMap consistency") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      NonEmptyMap.fromMap(nem.toSortedMap) should ===(Some(NonEmptyMap.fromMapUnsafe(nem.toSortedMap)))
    }
  }

  test("fromMapUnsafe empty map") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyMap.fromMapUnsafe(SortedMap.empty[String, Int])
    }
  }

  test("+ consistent with Map") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      nem.add(i).toSortedMap should ===(nem.toSortedMap + i)
    }
  }

  test("NonEmptyMap#size and length is consistent with Map#size") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.size should ===(nem.toSortedMap.size.toLong)
      nem.length should ===(nem.toSortedMap.size)
    }
  }

  test("NonEmptyMap#toNonEmptyList is consistent with Map#toList and creating NonEmptyList from it") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.toNel should ===(NonEmptyList.fromListUnsafe(nem.toSortedMap.toList))
    }
  }

  test("NonEmptyMap#updateWith identity should be a no-op") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      nem.add(i) should ===(nem.add(i).updateWith(i._1)(identity))
    }
  }

  test("NonEmptyMap#updateWith on existing value should behave like Option#map on the same value") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      nem.add(i).lookup(i._1).map(_ + 1) should ===(nem.add(i).updateWith(i._1)(_ + 1).lookup(i._1))
    }
  }

  test("NonEmptyMap#updateWith should not act when key is missing") {
    val single = NonEmptyMap[String, Int](("here", 1), SortedMap())
    single.lookup("notHere") should ===(single.updateWith("notHere")(_ => 1).lookup("notHere"))
  }

}
