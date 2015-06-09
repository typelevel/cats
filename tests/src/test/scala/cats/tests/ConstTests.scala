package cats
package tests

import algebra.laws.{GroupLaws, OrderLaws}

import cats.data.Const
import cats.laws.discipline.{ApplyTests, ApplicativeTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary.constArbitrary

class ConstTests extends CatsSuite {
  checkAll("Const[String, Int]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Const[String, ?]]", SerializableTests.serializable(Applicative[Const[String, ?]]))

  checkAll("Const[String, Int] with Option", TraverseTests[Const[String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Const[String, ?]]", SerializableTests.serializable(Traverse[Const[String, ?]]))

  checkAll("Apply[Const[String, Int]]", ApplyTests[Const[String, ?]].apply[Int, Int, Int])
  checkAll("Apply[Const[String, ?]]", SerializableTests.serializable(Apply[Const[String, ?]]))

  // Algebra checks for Serializability of instances as part of the laws
  checkAll("PartialOrder[Const[Int, String]]", OrderLaws[Const[Int, String]].partialOrder)
  checkAll("Monoid[Const[Int, String]]", GroupLaws[Const[Int, String]].monoid)
}
