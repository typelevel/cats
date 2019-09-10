package cats
package laws
package discipline

import org.scalacheck.Arbitrary

private[discipline] object ScalaVersionSpecific {
  trait ArbitraryInstances {
    implicit def catsLawsArbitraryForZipStream[A](implicit A: Arbitrary[A]): Arbitrary[ZipStream[A]] =
      Arbitrary(implicitly[Arbitrary[Stream[A]]].arbitrary.map(v => new ZipStream(v)))
  }
}
