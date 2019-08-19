package cats.laws.discipline

import cats.data.{NonEmptyLazyList, ZipLazyList, ZipStream}
import cats.data.NonEmptyLazyList.ZipNonEmptyLazyList
import org.scalacheck.{Arbitrary, Cogen}

private[discipline] object ScalaVersionSpecific {

  trait ArbitraryInstances {
    @deprecated("2.0.0-RC2", "Use catsLawsArbitraryForZipLazyList")
    implicit def catsLawsArbitraryForZipStream[A](implicit A: Arbitrary[A]): Arbitrary[ZipStream[A]] =
      Arbitrary(implicitly[Arbitrary[Stream[A]]].arbitrary.map(v => new ZipStream(v)))

    implicit def catsLawsArbitraryForZipLazyList[A](implicit A: Arbitrary[A]): Arbitrary[ZipLazyList[A]] =
      Arbitrary(implicitly[Arbitrary[LazyList[A]]].arbitrary.map(v => new ZipLazyList(v)))

    implicit def catsLawsArbitraryForNonEmptyLazyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyLazyList[A]] =
      Arbitrary(
        implicitly[Arbitrary[LazyList[A]]].arbitrary
          .flatMap(fa => A.arbitrary.map(a => NonEmptyLazyList.fromLazyListPrepend(a, fa)))
      )

    implicit def catsLawsCogenForNonEmptyLazyList[A](implicit A: Cogen[A]): Cogen[NonEmptyLazyList[A]] =
      Cogen[LazyList[A]].contramap(_.toLazyList)

    implicit def catsLawsArbitraryForZipNonEmptyLazyList[A](
      implicit A: Arbitrary[A]
    ): Arbitrary[ZipNonEmptyLazyList[A]] =
      Arbitrary(implicitly[Arbitrary[NonEmptyLazyList[A]]].arbitrary.map(nev => new ZipNonEmptyLazyList(nev)))

  }
}
