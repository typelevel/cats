package cats
package tests

import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

import scala.collection.immutable.Queue

class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]

  checkAll("Eq", ContravariantMonoidalTests[Eq].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Eq]", SerializableTests.serializable(ContravariantMonoidal[Eq]))

  test("The Eq instance for tuples of A is not ambiguous when A has a Hash and a PartialOrder") {

    import cats.kernel.{Hash, PartialOrder}

    trait A
    implicit def po: PartialOrder[A] = ???
    implicit def ho: Hash[A] = ???

    lazy val a2 = implicitly[Eq[(A, A)]]
    lazy val b2 = implicitly[Eq[(List[A], List[A])]]
    lazy val c2 = implicitly[Eq[(Set[A], Set[A])]]
    lazy val d2 = implicitly[Eq[(Vector[A], Vector[A])]]
    lazy val e2 = implicitly[Eq[(Queue[A], Queue[A])]]

    lazy val a3 = implicitly[Eq[(A, A, A)]]
    lazy val b3 = implicitly[Eq[(List[A], List[A], List[A])]]
    lazy val c3 = implicitly[Eq[(Set[A], Set[A], Set[A])]]
    lazy val d3 = implicitly[Eq[(Vector[A], Vector[A], Vector[A])]]
    lazy val e3 = implicitly[Eq[(Queue[A], Queue[A], Queue[A])]]
  }
}
