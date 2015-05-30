package cats
package tests

import cats.data.Validated
import cats.std.string._
import cats.laws.discipline.{TraverseTests, ApplicativeTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class ValidatedTests extends CatsSuite {
  checkAll("Validated[String, Int]", ApplicativeTests[Validated[String,?]].applicative[Int, Int, Int])
  checkAll("Applicative[Validated[String,?]]", SerializableTests.serializable(Applicative[Validated[String,?]]))

  checkAll("Validated[String, Int] with Option", TraverseTests[Validated[String,?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Validated[String,?]]", SerializableTests.serializable(Traverse[Validated[String,?]]))
}
