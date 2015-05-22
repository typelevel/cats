package cats
package tests

import cats.data.Validated
import cats.std.string._
import cats.laws.discipline.{ApplicativeTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class ValidatedTests extends CatsSuite {

  checkAll("Applicative[Validated[String, Int]]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Serializable[Applicative[Validated[String,?]]]", SerializableTests.serializable(Applicative[Validated[String,?]]))
}
