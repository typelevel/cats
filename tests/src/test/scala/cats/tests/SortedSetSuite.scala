package cats.tests

import cats.{SemigroupK, Semigroupal, Show}
import cats.kernel.{Order, PartialOrder}
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, HashTests, OrderTests, PartialOrderTests}
import cats.kernel.{BoundedSemilattice, Semilattice}
import cats.laws._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{FoldableTests, SemigroupKTests, SemigroupalTests, SerializableTests, ShortCircuitingTests}
import cats.syntax.show._
import scala.collection.immutable.SortedSet

class SortedSetSuite extends CatsSuite {
  implicit val iso: Isomorphisms[SortedSet] = SortedSetIsomorphism

  checkAll("SortedSet[Int]", SemigroupKTests[SortedSet].semigroupK[Int])
  checkAll("SortedSet[Int]", SemigroupalTests[SortedSet].semigroupal[Int, Int, Int])
  checkAll("SemigroupK[SortedSet]", SerializableTests.serializable(SemigroupK[SortedSet]))
  checkAll("Semigroupal[SortedSet]", SerializableTests.serializable(Semigroupal[SortedSet]))

  checkAll("SortedSet[Int]", FoldableTests[SortedSet].foldable[Int, Int])
  checkAll("Order[SortedSet[Int]]", OrderTests[SortedSet[Int]].order)
  checkAll("Order.reverse(Order[SortedSet[Int]])", OrderTests(Order.reverse(Order[SortedSet[Int]])).order)
  checkAll("PartialOrder[SortedSet[Int]]", PartialOrderTests[SortedSet[Int]].partialOrder)
  checkAll("PartialOrder.reverse(PartialOrder[SortedSet[Int]])",
           PartialOrderTests(PartialOrder.reverse(PartialOrder[SortedSet[Int]])).partialOrder
  )
  checkAll(
    "PartialOrder.reverse(PartialOrder.reverse(PartialOrder[SortedSet[Int]]))",
    PartialOrderTests(PartialOrder.reverse(PartialOrder.reverse(PartialOrder[SortedSet[Int]]))).partialOrder
  )

  checkAll("BoundedSemilattice[SortedSet[String]]", BoundedSemilatticeTests[SortedSet[String]].boundedSemilattice)
  checkAll("BoundedSemilattice[SortedSet[String]]",
           SerializableTests.serializable(BoundedSemilattice[SortedSet[String]])
  )

  checkAll("Semilattice.asMeetPartialOrder[SortedSet[Int]]",
           PartialOrderTests(Semilattice.asMeetPartialOrder[SortedSet[Int]]).partialOrder
  )
  checkAll("Semilattice.asJoinPartialOrder[SortedSet[Int]]",
           PartialOrderTests(Semilattice.asJoinPartialOrder[SortedSet[Int]]).partialOrder
  )
  checkAll("Hash[SortedSet[Int]]", HashTests[SortedSet[Int]].hash)

  checkAll("SortedSet[Int]", ShortCircuitingTests[SortedSet].foldable[Int])

  test("show keeps separate entries for items that map to identical strings") {
    // note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = Show.show(_ => "1")
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because SortedSet.map combines
    // duplicate items in the codomain.
    SortedSet(1, 2, 3).show should ===("SortedSet(1, 1, 1)")
  }
}

object SortedSetIsomorphism extends Isomorphisms[SortedSet] {

  override def associativity[A, B, C](
    fs: (SortedSet[(A, (B, C))], SortedSet[((A, B), C)])
  ): IsEq[SortedSet[(A, B, C)]] = {
    implicit val ord: Ordering[(A, B, C)] = Ordering.by[(A, B, C), ((A, B), C)] { case (a, b, c) => ((a, b), c) }(
      fs._2.ordering
    )

    fs._1.map { case (a, (b, c)) => (a, b, c) } <->
      fs._2.map { case ((a, b), c) => (a, b, c) }
  }

  override def leftIdentity[A](fs: (SortedSet[(Unit, A)], SortedSet[A])): IsEq[SortedSet[A]] = {
    implicit val ordering: Ordering[A] = fs._2.ordering
    fs._1.map(_._2) <-> fs._2
  }

  override def rightIdentity[A](fs: (SortedSet[(A, Unit)], SortedSet[A])): IsEq[SortedSet[A]] = {
    implicit val ordering: Ordering[A] = fs._2.ordering
    fs._1.map(_._1) <-> fs._2
  }
}
