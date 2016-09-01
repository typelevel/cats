package cats.kernel
package laws

import cats.kernel._
import cats.kernel.instances.option._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object GroupLaws {
  def apply[A : Eq : Arbitrary]: GroupLaws[A] = new GroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait GroupLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  // groups

  def semigroup(implicit A: Semigroup[A]): GroupProperties = new GroupProperties(
    name = "semigroup",
    parents = Nil,
    Rules.serializable(A),
    Rules.associativity(A.combine),
    Rules.repeat1("combineN")(A.combineN),
    Rules.repeat2("combineN", "|+|")(A.combineN)(A.combine),
    "combineAllOption" -> forAll { (xs: Vector[A]) =>
      A.combineAllOption(xs) ?== xs.reduceOption(A.combine)
    }
  )

  def band(implicit A: Band[A]): GroupProperties = new GroupProperties(
    name = "band",
    parents = List(semigroup),
    Rules.idempotence(A.combine),
    "isIdempotent" -> Semigroup.isIdempotent[A]
  )

  def commutativeSemigroup(implicit A: CommutativeSemigroup[A]): GroupProperties = new GroupProperties(
    name = "commutative semigroup",
    parents = List(semigroup),
    Rules.commutative(A.combine)
  )

  def semilattice(implicit A: Semilattice[A]): GroupProperties = new GroupProperties(
    name = "semilattice",
    parents = List(band, commutativeSemigroup)
  )

  def monoid(implicit A: Monoid[A]): GroupProperties = new GroupProperties(
    name = "monoid",
    parents = List(semigroup),
    Rules.leftIdentity(A.empty)(A.combine),
    Rules.rightIdentity(A.empty)(A.combine),
    Rules.repeat0("combineN", "id", A.empty)(A.combineN),
    Rules.collect0("combineAll", "id", A.empty)(A.combineAll),
    Rules.isId("isEmpty", A.empty)(A.isEmpty),
    "combineAll" -> forAll { (xs: Vector[A]) =>
      A.combineAll(xs) ?== (A.empty +: xs).reduce(A.combine)
    }
  )

  def commutativeMonoid(implicit A: CommutativeMonoid[A]): GroupProperties = new GroupProperties(
    name = "commutative monoid",
    parents = List(monoid, commutativeSemigroup)
  )

  def boundedSemilattice(implicit A: BoundedSemilattice[A]): GroupProperties = new GroupProperties(
    name = "boundedSemilattice",
    parents = List(commutativeMonoid, semilattice)
  )

  def group(implicit A: Group[A]): GroupProperties = new GroupProperties(
    name = "group",
    parents = List(monoid),
    Rules.leftInverse(A.empty)(A.combine)(A.inverse),
    Rules.rightInverse(A.empty)(A.combine)(A.inverse),
    Rules.consistentInverse("remove")(A.remove)(A.combine)(A.inverse)
  )

  def commutativeGroup(implicit A: CommutativeGroup[A]): GroupProperties = new GroupProperties(
    name = "commutative group",
    parents = List(group, commutativeMonoid)
  )

  // property classes

  class GroupProperties(
    val name: String,
    val parents: Seq[GroupProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
