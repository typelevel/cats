package cats
package tests

import cats.functor._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class OrderingTests extends CatsSuite {

  Invariant[Ordering]
  Contravariant[Ordering]
  Cartesian[Ordering]
  ContravariantCartesian[Ordering]

  checkAll("Contravariant[Ordering]", ContravariantTests[Ordering].contravariant[Int, Int, Int])
  checkAll("Cartesian[Ordering]", CartesianTests[Ordering].cartesian[Int, Int, Int])
  checkAll("Contravariant[Ordering]", SerializableTests.serializable(Contravariant[Ordering]))
}
