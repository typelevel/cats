package cats.tests

import cats.{Align, FlatMap, FunctorFilter, MonoidK, Semigroupal, Show, UnorderedTraverse}
import cats.arrow.Compose
import cats.kernel.{CommutativeMonoid, Monoid}
import cats.kernel.instances.StaticMethods.wrapMutableMap
import cats.kernel.laws.discipline.{CommutativeMonoidTests, HashTests, MonoidTests}
import cats.laws.discipline.{
  AlignTests,
  ComposeTests,
  FlatMapTests,
  FunctorFilterTests,
  MonoidKTests,
  SemigroupalTests,
  SerializableTests,
  UnorderedTraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._
import org.scalacheck.Prop._

class MapSuite extends CatsSuite {

  checkAll("Map[Int, Int]", SemigroupalTests[Map[Int, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Map[Int, *]]", SerializableTests.serializable(Semigroupal[Map[Int, *]]))

  checkAll("Map[Int, Int]", FlatMapTests[Map[Int, *]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Map[Int, *]]", SerializableTests.serializable(FlatMap[Map[Int, *]]))

  checkAll("Map[Int, Int] with Option",
           UnorderedTraverseTests[Map[Int, *]].unorderedTraverse[Int, Int, Int, Option, Option]
  )
  checkAll("UnorderedTraverse[Map[Int, *]]", SerializableTests.serializable(UnorderedTraverse[Map[Int, *]]))

  checkAll("Map[Int, Int]", FunctorFilterTests[Map[Int, *]].functorFilter[Int, Int, Int])
  checkAll("FunctorFilter[Map[Int, *]]", SerializableTests.serializable(FunctorFilter[Map[Int, *]]))

  checkAll("Map[Int, Long]", ComposeTests[Map].compose[Int, Long, String, Double])
  checkAll("Compose[Map]", SerializableTests.serializable(Compose[Map]))

  checkAll("Map[Int, Int]", MonoidKTests[Map[Int, *]].monoidK[Int])
  checkAll("MonoidK[Map[Int, *]]", SerializableTests.serializable(MonoidK[Map[Int, *]]))

  checkAll("Map[Int, Int]", AlignTests[Map[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[Map]", SerializableTests.serializable(Align[Map[Int, *]]))

  checkAll("Hash[Map[Int, String]]", HashTests[Map[Int, String]].hash)
  checkAll("CommutativeMonoid[Map[String, Int]]", CommutativeMonoidTests[Map[String, Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[Map[String, Int]]", SerializableTests.serializable(CommutativeMonoid[Map[String, Int]]))
  checkAll("Monoid[Map[String, String]]", MonoidTests[Map[String, String]].monoid)
  checkAll("Monoid[Map[String, String]]", SerializableTests.serializable(Monoid[Map[String, String]]))

  test("show isn't empty and is formatted as expected") {
    forAll { (map: Map[Int, String]) =>
      assert(map.show.nonEmpty === (true))
      assert(map.show.startsWith("Map(") === (true))
      assert(map.show === (implicitly[Show[Map[Int, String]]].show(map)))
    }
  }

  {
    val m = wrapMutableMap(scala.collection.mutable.Map(1 -> "one", 2 -> "two"))
    checkAll("WrappedMutableMap", SerializableTests.serializable(m))
  }
}
