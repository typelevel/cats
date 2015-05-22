package cats
package tests

import cats.data.Const
import cats.laws.discipline.{ApplicativeTests, SerializableTests}

class ConstTests extends CatsSuite {
  checkAll("Applicative[Const[String, Int]]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])
  checkAll("Serializable[Const[String, ?]]", SerializableTests.serializable(Applicative[Const[String, ?]]))
}
