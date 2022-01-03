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
import cats.data.OrderedSet
import cats.syntax.eq._

class OrderedSetSuite extends CatsSuite {
  implicit val iso: Isomorphisms[OrderedSet] = OrderedSetIsomorphism

  checkAll("OrderedSet[Int]", SemigroupKTests[OrderedSet].semigroupK[Int])
  checkAll("OrderedSet[Int]", SemigroupalTests[OrderedSet].semigroupal[Int, Int, Int])
  checkAll("SemigroupK[OrderedSet]", SerializableTests.serializable(SemigroupK[OrderedSet]))
  checkAll("Semigroupal[OrderedSet]", SerializableTests.serializable(Semigroupal[OrderedSet]))

  checkAll("OrderedSet[Int]", FoldableTests[OrderedSet].foldable[Int, Int])
  checkAll("Order[OrderedSet[Int]]", OrderTests[OrderedSet[Int]].order)
  checkAll("Order.reverse(Order[OrderedSet[Int]])", OrderTests(Order.reverse(Order[OrderedSet[Int]])).order)
  checkAll("PartialOrder[OrderedSet[Int]]", PartialOrderTests[OrderedSet[Int]].partialOrder)
  checkAll("PartialOrder.reverse(PartialOrder[OrderedSet[Int]])",
           PartialOrderTests(PartialOrder.reverse(PartialOrder[OrderedSet[Int]])).partialOrder
  )
  checkAll(
    "PartialOrder.reverse(PartialOrder.reverse(PartialOrder[OrderedSet[Int]]))",
    PartialOrderTests(PartialOrder.reverse(PartialOrder.reverse(PartialOrder[OrderedSet[Int]]))).partialOrder
  )

  checkAll("BoundedSemilattice[OrderedSet[String]]", BoundedSemilatticeTests[OrderedSet[String]].boundedSemilattice)
  checkAll("BoundedSemilattice[OrderedSet[String]]",
           SerializableTests.serializable(BoundedSemilattice[OrderedSet[String]])
  )

  checkAll("Semilattice.asMeetPartialOrder[OrderedSet[Int]]",
           PartialOrderTests(Semilattice.asMeetPartialOrder[OrderedSet[Int]]).partialOrder
  )
  checkAll("Semilattice.asJoinPartialOrder[OrderedSet[Int]]",
           PartialOrderTests(Semilattice.asJoinPartialOrder[OrderedSet[Int]]).partialOrder
  )
  checkAll("Hash[OrderedSet[Int]]", HashTests[OrderedSet[Int]].hash)

  checkAll("OrderedSet[Int]", ShortCircuitingTests[OrderedSet].foldable[Int])

  test("show keeps separate entries for items that map to identical strings") {
    // note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = Show.show(_ => "1")
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because OrderedSet.map combines
    // duplicate items in the codomain.
    assert(OrderedSet.of(1, 2, 3).show === "OrderedSet(1, 1, 1)")
  }
}

object OrderedSetIsomorphism extends Isomorphisms[OrderedSet] {

  override def associativity[A, B, C](
    fs: (OrderedSet[(A, (B, C))], OrderedSet[((A, B), C)])
  ): IsEq[OrderedSet[(A, B, C)]] = {
    implicit val ordering: Ordering[(A, (B, C))] =
      fs._1.toSortedSet.ordering
    implicit val order: Order[(A, B, C)] =
      Order.fromOrdering(Ordering.by[(A, B, C), (A, (B, C))] { case (a, b, c) => (a, (b, c)) })

    fs._1.map { case (a, (b, c)) => (a, b, c) } <->
      fs._2.map { case ((a, b), c) => (a, b, c) }
  }

  override def leftIdentity[A](fs: (OrderedSet[(Unit, A)], OrderedSet[A])): IsEq[OrderedSet[A]] = {
    implicit val order: Order[A] = Order.fromOrdering(fs._2.toSortedSet.ordering)
    fs._1.map(_._2) <-> fs._2
  }

  override def rightIdentity[A](fs: (OrderedSet[(A, Unit)], OrderedSet[A])): IsEq[OrderedSet[A]] = {
    implicit val order: Order[A] = Order.fromOrdering(fs._2.toSortedSet.ordering)
    fs._1.map(_._1) <-> fs._2
  }
}
