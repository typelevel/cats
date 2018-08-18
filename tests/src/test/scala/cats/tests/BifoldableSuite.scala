package cats
package tests

import cats.laws.discipline.arbitrary._
import cats.data.Writer
import cats.laws.discipline.{BifoldableTests, SerializableTests}

class BifoldableSuite extends CatsSuite {
  type EitherEither[A, B] = Either[Either[A, B], Either[A, B]]
  val eitherComposeEither: Bifoldable[EitherEither] =
    Bifoldable[Either].compose[Either]

  checkAll("Either compose Either", BifoldableTests(eitherComposeEither).bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Either compose Either]", SerializableTests.serializable(eitherComposeEither))

  test("bitraverse_ consistent with bitraverse"){
    forAll { (e: Either[Int, Long], f: Int => Writer[String, Boolean], g: Long => Writer[String, Double]) =>
      e.bitraverse_(f, g) should ===(e.bitraverse(f, g).void)

    }
  }
}
