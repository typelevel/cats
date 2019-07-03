package cats.laws.discipline

import cats.data.NonEmptyLazyList
import org.scalacheck.{Arbitrary, Cogen}

object VersionSpecific {

  trait ArbitraryInstances {

    implicit def catsLawsArbitraryForNonEmptyLazyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyLazyList[A]] = {
      Arbitrary(implicitly[Arbitrary[LazyList[A]]].arbitrary.
        flatMap(fa => A.arbitrary.map(a => NonEmptyLazyList.fromLazyListPrepend(a, fa))))
    }
    implicit def catsLawsCogenForNonEmptyLazyList[A](implicit A: Cogen[A]): Cogen[NonEmptyLazyList[A]] =
      Cogen[LazyList[A]].contramap(_.toLazyList)

  }
}