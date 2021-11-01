package alleycats.tests

import alleycats.laws.discipline._
import alleycats.std.all._
import cats.{Alternative, Foldable}
import cats.instances.all._
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{AlternativeTests, ShortCircuitingTests, TraverseFilterTests}

class SetSuite extends AlleycatsSuite {
  implicit val iso: Isomorphisms[Set] = Isomorphisms.invariant[Set](alleyCatsStdSetMonad)

  checkAll("FlatMapRec[Set]", FlatMapRecTests[Set].tailRecM[Int])

  checkAll("Foldable[Set]", SerializableTests.serializable(Foldable[Set]))

  checkAll("TraverseFilter[Set]", TraverseFilterTests[Set].traverseFilter[Int, Int, Int])

  checkAll("Set[Int]", AlternativeTests[Set].alternative[Int, Int, Int])
  checkAll("Alternative[Set]", SerializableTests.serializable(Alternative[Set]))

  checkAll("Set[Int]", ShortCircuitingTests[Set].traverseFilter[Int])
}
