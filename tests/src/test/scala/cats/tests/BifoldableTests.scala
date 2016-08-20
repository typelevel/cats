package cats
package tests

import cats.data.Xor
import cats.laws.discipline.{BifoldableTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class BifoldableTest extends CatsSuite {
  type EitherXor[A, B] = Either[Xor[A, B], Xor[A, B]]
  val eitherComposeXor: Bifoldable[EitherXor] =
    Bifoldable[Either].compose[Xor]

  checkAll("Either compose Xor", BifoldableTests(eitherComposeXor).bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Either compose Xor]", SerializableTests.serializable(eitherComposeXor))
}
