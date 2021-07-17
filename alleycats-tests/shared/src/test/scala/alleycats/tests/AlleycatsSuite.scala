package alleycats.tests

import alleycats.std.MapInstances
import cats._
import cats.instances.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Test.Parameters

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Alleycats tests. Derived from Cats.
 */
trait AlleycatsSuite extends munit.DisciplineSuite with TestSettings with TestInstances with MapInstances {
  implicit override def scalaCheckTestParameters: Parameters =
    checkConfiguration

  implicit def EqIterable[A: Eq]: Eq[Iterable[A]] = Eq.by(_.toList)
}

sealed trait TestInstances {
  implicit val arbObject: Arbitrary[Object] =
    // with some probability we select from a small set of objects
    // otherwise make a totally new one
    // courtesy of @johnynek
    Arbitrary(
      Gen.oneOf(
        Gen.oneOf(List.fill(5)(new Object)),
        Arbitrary.arbUnit.arbitrary.map(_ => new Object)
      )
    )

  implicit val arbObjectF: Arbitrary[Object => Object] =
    Arbitrary {
      for {
        obj <- arbitrary[Object]
      } yield _ => obj
    }
}
