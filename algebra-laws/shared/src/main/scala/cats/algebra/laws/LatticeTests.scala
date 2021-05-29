package cats.algebra.laws

import cats.algebra._
import cats.algebra.lattice._
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, SemilatticeTests}
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object LatticeTests {
  def apply[A: Eq: Arbitrary] = new LatticeTests[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LatticeTests[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def joinSemilattice(implicit A: JoinSemilattice[A]) = new LatticeProperties(
    name = "joinSemilattice",
    parents = Nil,
    join = Some(SemilatticeTests[A](A.joinSemilattice).semilattice),
    meet = None,
    Rules.serializable(A)
  )

  def meetSemilattice(implicit A: MeetSemilattice[A]) = new LatticeProperties(
    name = "meetSemilattice",
    parents = Nil,
    join = None,
    meet = Some(SemilatticeTests[A](A.meetSemilattice).semilattice),
    Rules.serializable(A)
  )

  def lattice(implicit A: Lattice[A]) = new LatticeProperties(
    name = "lattice",
    parents = Seq(joinSemilattice, meetSemilattice),
    join = Some(SemilatticeTests[A](A.joinSemilattice).semilattice),
    meet = Some(SemilatticeTests[A](A.meetSemilattice).semilattice),
    "absorption" -> forAll { (x: A, y: A) =>
      (A.join(x, A.meet(x, y)) ?== x) && (A.meet(x, A.join(x, y)) ?== x)
    }
  )

  def distributiveLattice(implicit A: DistributiveLattice[A]) = new LatticeProperties(
    name = "distributiveLattice",
    parents = Seq(lattice),
    join = Some(SemilatticeTests[A](A.joinSemilattice).semilattice),
    meet = Some(SemilatticeTests[A](A.meetSemilattice).semilattice),
    "distributive" -> forAll { (x: A, y: A, z: A) =>
      (A.join(x, A.meet(y, z)) ?== A.meet(A.join(x, y), A.join(x, z))) &&
      (A.meet(x, A.join(y, z)) ?== A.join(A.meet(x, y), A.meet(x, z)))
    }
  )

  def boundedJoinSemilattice(implicit A: BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedJoinSemilattice",
    parents = Seq(joinSemilattice),
    join = Some(BoundedSemilatticeTests(A.joinSemilattice).boundedSemilattice),
    meet = None
  )

  def boundedMeetSemilattice(implicit A: BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedMeetSemilattice",
    parents = Seq(meetSemilattice),
    join = None,
    meet = Some(BoundedSemilatticeTests(A.meetSemilattice).boundedSemilattice)
  )

  def boundedJoinLattice(implicit A: Lattice[A] with BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedJoinLattice",
    parents = Seq(boundedJoinSemilattice, lattice),
    join = Some(BoundedSemilatticeTests(A.joinSemilattice).boundedSemilattice),
    meet = Some(SemilatticeTests(A.meetSemilattice).semilattice)
  )

  def boundedMeetLattice(implicit A: Lattice[A] with BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedMeetLattice",
    parents = Seq(boundedMeetSemilattice, lattice),
    join = Some(SemilatticeTests(A.joinSemilattice).semilattice),
    meet = Some(BoundedSemilatticeTests(A.meetSemilattice).boundedSemilattice)
  )

  def boundedLattice(implicit A: BoundedLattice[A]) = new LatticeProperties(
    name = "boundedLattice",
    parents = Seq(boundedJoinSemilattice, boundedMeetSemilattice, lattice),
    join = Some(BoundedSemilatticeTests(A.joinSemilattice).boundedSemilattice),
    meet = Some(BoundedSemilatticeTests(A.meetSemilattice).boundedSemilattice)
  )

  def boundedDistributiveLattice(implicit A: BoundedDistributiveLattice[A]) = new LatticeProperties(
    name = "boundedLattice",
    parents = Seq(boundedLattice, distributiveLattice),
    join = Some(BoundedSemilatticeTests(A.joinSemilattice).boundedSemilattice),
    meet = Some(BoundedSemilatticeTests(A.meetSemilattice).boundedSemilattice)
  )

  class LatticeProperties(
    val name: String,
    val parents: Seq[LatticeProperties],
    val join: Option[Laws#RuleSet],
    val meet: Option[Laws#RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet {
    private val _m = meet.map { "meet" -> _ }
    private val _j = join.map { "join" -> _ }

    val bases = _m.toList ::: _j.toList
  }

}
