package cats
package tests

import cats.data.NonEmptyLazyList
import cats.instances.all._
import cats.kernel.laws.discipline.{EqTests, HashTests, PartialOrderTests, SerializableTests}
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
  
  {
    implicit val partialOrder = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyLazyList[ListWrapper[Int]]", PartialOrderTests[NonEmptyLazyList[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyLazyList[ListWrapper[Int]]",
      SerializableTests.serializable(PartialOrder[NonEmptyLazyList[ListWrapper[Int]]]))
  }

  {
    implicit val eqv = ListWrapper.eqv[Int]
    checkAll("NonEmptyLazyList[ListWrapper[Int]]", EqTests[NonEmptyLazyList[ListWrapper[Int]]].eqv)
    checkAll("Eq[NonEmptyLazyList[ListWrapper[Int]]", SerializableTests.serializable(Eq[NonEmptyLazyList[ListWrapper[Int]]]))
  }
}