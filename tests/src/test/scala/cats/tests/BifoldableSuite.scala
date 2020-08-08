package cats.tests

import cats.Bifoldable
import cats.laws.discipline.{BifoldableTests, SerializableTests}
import cats.syntax.either._
import cats.syntax.eq._

class BifoldableSuite extends CatsSuite {
  type EitherEither[A, B] = Either[Either[A, B], Either[A, B]]
  val eitherComposeEither: Bifoldable[EitherEither] =
    Bifoldable[Either].compose[Either]

  checkAll("Either compose Either", BifoldableTests(eitherComposeEither).bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Either compose Either]", SerializableTests.serializable(eitherComposeEither))

  test("bifold works for 2 monoids") {
    assert(Bifoldable[Either].bifold(Either.right[Int, String]("something")) === ((0, "something")))
    assert(Bifoldable[Either].bifold(Either.left[Int, String](5)) === ((5, "")))
  }
}
