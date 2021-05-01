package algebra.laws

import algebra._
import algebra.lattice._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object DeMorganLaws {
  def apply[A: Eq: Arbitrary: LatticeLaws] = new DeMorganLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def LL = implicitly[LatticeLaws[A]]
  }
}
/* TODO:
 *  This is separated for LogicLaws for binary compatibility reasons.
 *  Merge with LogicLaws when possible.
 */
trait DeMorganLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  def LL: LatticeLaws[A]

  def logic(implicit A: Logic[A]) = new DeMorganProperties(
    name = "logic",
    parents = Seq(),
    ll = LL.boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "¬(x∨y) = ¬x∧¬y" -> forAll { (x: A, y: A) => A.not(A.or(x, y)) ?== A.and(A.not(x), A.not(y)) },
    "¬(x∧y) = ¬¬(¬x∨¬y)" -> forAll { (x: A, y: A) => A.not(A.and(x, y)) ?== A.not(A.not(A.or(A.not(x), A.not(y)))) }
  )

  def deMorgan(implicit A: DeMorgan[A]) = new DeMorganProperties(
    name = "deMorgan",
    parents = Seq(logic),
    ll = LL.boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "involutive" -> forAll { (x: A) => A.not(A.not(x)) ?== x }
  )

  class DeMorganProperties(
    val name: String,
    val parents: Seq[DeMorganProperties],
    val ll: LatticeLaws[A]#LatticeProperties,
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("lattice" -> ll)
  }

}
