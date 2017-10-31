package cats
package tests


import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import cats.laws.discipline.eq._

class OrderingSuite extends CatsSuite {

  Invariant[Ordering]
  Contravariant[Ordering]
  Semigroupal[Ordering]
  ContravariantSemigroupal[Ordering]
  Divisible[Ordering]

  checkAll("Contravariant[Ordering]", ContravariantTests[Ordering].contravariant[Int, Int, Int])
  checkAll("Semigroupal[Ordering]", SemigroupalTests[Ordering].semigroupal[Int, Int, Int])
  checkAll("Divisible[Ordering]", DivisibleTests[Ordering].divisible[Int, Int, Int])
  checkAll("Divisible[Ordering]", SerializableTests.serializable(Divisible[Ordering]))
}
