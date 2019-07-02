package cats
package tests

import cats.data.NonEmptyLazyList
import cats.instances.all._
import cats.kernel.laws.discipline.{HashTests, SerializableTests}
import org.scalacheck.{Arbitrary, Cogen}



class NonEmptyLazyListSuite extends NonEmptyDataTypeSuite[NonEmptyLazyList]("LazyList") {
  implicit def arbitraryFA[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyLazyList[A]] = {
    Arbitrary(implicitly[Arbitrary[LazyList[A]]].arbitrary.
      flatMap(fa => A.arbitrary.map(a => NonEmptyLazyList.fromLazyListPrepend(a, fa))))
  }
  implicit def cogenFA[A](implicit A: Cogen[A]): Cogen[NonEmptyLazyList[A]] =
    Cogen[LazyList[A]].contramap(_.toLazyList)

  checkAll(s"NonEmptyLazyList[Int]", HashTests[NonEmptyLazyList[Int]].hash)
  checkAll(s"Hash[NonEmptyLazyList[Int]]", SerializableTests.serializable(Hash[NonEmptyLazyList[Int]]))
}