package cats
package tests

import cats.functor._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class EquivTests extends CatsSuite {

  Invariant[Equiv]
  Contravariant[Equiv]
  Cartesian[Equiv]
  ContravariantCartesian[Equiv]

  checkAll("Contravariant[Equiv]", ContravariantTests[Equiv].contravariant[Int, Int, Int])
  checkAll("Cartesian[Equiv]", CartesianTests[Equiv].cartesian[Int, Int, Int])
  checkAll("Contravariant[Equiv]", SerializableTests.serializable(Contravariant[Equiv]))
}
