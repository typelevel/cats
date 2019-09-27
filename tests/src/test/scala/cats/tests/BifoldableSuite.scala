package cats
package tests

import cats.laws.discipline.{BifoldableTests, SerializableTests}

class BifoldableSuite extends CatsSuite {
  type EitherEither[A, B] = Either[Either[A, B], Either[A, B]]
  val eitherComposeEither: Bifoldable[EitherEither] =
    Bifoldable[Either].compose[Either]

  checkAll("Either compose Either", BifoldableTests(eitherComposeEither).bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Either compose Either]", SerializableTests.serializable(eitherComposeEither))

  test("bifold works for 2 monoids") {
    Bifoldable[Either].bifold(Either.right[Int, String]("something")) should ===((0, "something"))
    Bifoldable[Either].bifold(Either.left[Int, String](5)) should ===((5, ""))
  }
}
