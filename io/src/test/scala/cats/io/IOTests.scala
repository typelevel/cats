package cats
package io

import cats.laws.discipline.{MonadTests, SerializableTests}
import cats.tests.CatsSuite
import cats.laws.discipline.eq.tuple3Eq

class IOTests extends CatsSuite with IOSuite {
  checkAll("IO", MonadTests[IO].monad[Int, String, Double])
  checkAll("Monad[IO]", SerializableTests.serializable(Monad[IO]))
}
