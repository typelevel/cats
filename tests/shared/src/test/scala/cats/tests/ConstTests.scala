package cats
package tests

import cats.data.Const
import cats.laws.discipline.{ApplicativeTests, SerializableTests}

class ConstTests extends CatsSuite {
  checkAll("Const[String, Int]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Const[String, ?]]", SerializableTests.serializable(Applicative[Const[String, ?]]))
}
