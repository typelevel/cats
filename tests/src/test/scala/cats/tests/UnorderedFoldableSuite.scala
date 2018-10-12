package cats
package tests

import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import cats.instances.all._

abstract class UnorderedFoldableSuite[F[_]: UnorderedFoldable](name: String)(
  implicit ArbFString: Arbitrary[F[String]]) extends CatsSuite with PropertyChecks {

  def iterator[T](fa: F[T]): Iterator[T]

  test(s"UnorderedFoldable[$name].count") {
    forAll { (fa: F[String], p: String => Boolean) =>
      fa.count(p) === iterator(fa).count(p).toLong
    }
  }
}

class UnorderedFoldableSetSuite extends UnorderedFoldableSuite[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
}

class UnorderedFoldableMapSuite extends UnorderedFoldableSuite[Map[String, ?]]("map") {
  def iterator[T](map: Map[String, T]): Iterator[T] = map.valuesIterator
}