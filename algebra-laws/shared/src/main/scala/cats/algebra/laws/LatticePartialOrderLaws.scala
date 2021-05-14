package cats.algebra.laws

import cats.algebra._
import cats.algebra.lattice._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import cats.algebra.instances.boolean._

object LatticePartialOrderLaws {
  def apply[A: Eq: Arbitrary] = new LatticePartialOrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LatticePartialOrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def joinSemilatticePartialOrder(implicit A: JoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "joinSemilatticePartialOrder",
      parents = Seq.empty,
      "join+lteqv" -> forAll { (x: A, y: A) =>
        P.lteqv(x, y) ?== P.eqv(y, A.join(x, y))
      }
    )

  def meetSemilatticePartialOrder(implicit A: MeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "meetSemilatticePartialOrder",
      parents = Seq.empty,
      "meet+lteqv" -> forAll { (x: A, y: A) =>
        P.lteqv(x, y) ?== P.eqv(x, A.meet(x, y))
      }
    )

  def latticePartialOrder(implicit A: Lattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "latticePartialOrder",
    parents = Seq(joinSemilatticePartialOrder, meetSemilatticePartialOrder)
  )

  def boundedJoinSemilatticePartialOrder(implicit A: BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedJoinSemilatticePartialOrder",
      parents = Seq(joinSemilatticePartialOrder),
      "lteqv+zero" -> forAll { (x: A) => A.zero ?<= x }
    )

  def boundedMeetSemilatticePartialOrder(implicit A: BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedMeetSemilatticePartialOrder",
      parents = Seq(meetSemilatticePartialOrder),
      "lteqv+one" -> forAll { (x: A) => x ?<= A.one }
    )

  def boundedBelowLatticePartialOrder(implicit A: Lattice[A] with BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedBelowLatticePartialOrder",
      parents = Seq(boundedJoinSemilatticePartialOrder, latticePartialOrder)
    )

  def boundedAboveLatticePartialOrder(implicit A: Lattice[A] with BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedAboveLatticePartialOrder",
      parents = Seq(boundedMeetSemilatticePartialOrder, latticePartialOrder)
    )

  def boundedLatticePartialOrder(implicit A: BoundedLattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "boundedLatticePartialOrder",
    parents = Seq(boundedJoinSemilatticePartialOrder, boundedMeetSemilatticePartialOrder)
  )

  class LatticePartialOrderProperties(
    val name: String,
    val parents: Seq[LatticePartialOrderProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Nil
  }

}
