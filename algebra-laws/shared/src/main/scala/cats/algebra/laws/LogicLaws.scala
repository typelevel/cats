package cats.algebra.laws

import cats.algebra._
import cats.algebra.lattice.{Bool, DeMorgan, GenBool, Heyting, Logic}
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object LogicLaws {
  def apply[A: Eq: Arbitrary] = new LogicLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LogicLaws[A] extends LatticeLaws[A] {

  def heyting(implicit A: Heyting[A]) = new LogicProperties(
    name = "heyting",
    parents = Seq(),
    ll = boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "consistent" -> forAll { (x: A) => A.and(x, A.complement(x)) ?== A.zero },
    "¬x = (x → 0)" -> forAll { (x: A) => A.complement(x) ?== A.imp(x, A.zero) },
    "x → x = 1" -> forAll { (x: A) => A.imp(x, x) ?== A.one },
    "if x → y and y → x then x=y" -> forAll { (x: A, y: A) =>
      (A.imp(x, y) ?!= A.one) || (A.imp(y, x) ?!= A.one) || (x ?== y)
    },
    "if (1 → x)=1 then x=1" -> forAll { (x: A) =>
      (A.imp(A.one, x) ?!= A.one) || (x ?== A.one)
    },
    "x → (y → x) = 1" -> forAll { (x: A, y: A) => A.imp(x, A.imp(y, x)) ?== A.one },
    "(x→(y→z)) → ((x→y)→(x→z)) = 1" -> forAll { (x: A, y: A, z: A) =>
      A.imp(A.imp(x, A.imp(y, z)), A.imp(A.imp(x, y), A.imp(x, z))) ?== A.one
    },
    "x∧y → x = 1" -> forAll { (x: A, y: A) => A.imp(A.and(x, y), x) ?== A.one },
    "x∧y → y = 1" -> forAll { (x: A, y: A) => A.imp(A.and(x, y), y) ?== A.one },
    "x → y → (x∧y) = 1" -> forAll { (x: A, y: A) => A.imp(x, A.imp(y, A.and(x, y))) ?== A.one },
    "x → x∨y" -> forAll { (x: A, y: A) => A.imp(x, A.or(x, y)) ?== A.one },
    "y → x∨y" -> forAll { (x: A, y: A) => A.imp(y, A.or(x, y)) ?== A.one },
    "(x → z) → ((y → z) → ((x | y) → z)) = 1" -> forAll { (x: A, y: A, z: A) =>
      A.imp(A.imp(x, z), A.imp(A.imp(y, z), A.imp(A.or(x, y), z))) ?== A.one
    },
    "(0 → x) = 1" -> forAll { (x: A) => A.imp(A.zero, x) ?== A.one }
  )

  def generalizedBool(implicit A: GenBool[A]) = new LogicProperties(
    name = "generalized bool",
    parents = Seq(),
    ll = new LatticeProperties(
      name = "lowerBoundedDistributiveLattice",
      parents = Seq(boundedJoinSemilattice, distributiveLattice),
      join = Some(boundedSemilattice(A.joinSemilattice)),
      meet = Some(semilattice(A.meetSemilattice))
    ),
    """x\y ∧ y = 0""" -> forAll { (x: A, y: A) =>
      A.and(A.without(x, y), y) ?== A.zero
    },
    """x\y ∨ y = x ∨ y""" -> forAll { (x: A, y: A) =>
      A.or(A.without(x, y), y) ?== A.or(x, y)
    }
  )

  def bool(implicit A: Bool[A]) = new LogicProperties(
    name = "bool",
    parents = Seq(heyting, generalizedBool),
    ll = boundedDistributiveLattice,
    "excluded middle" -> forAll { (x: A) => A.or(x, A.complement(x)) ?== A.one }
  )

  def logic(implicit A: Logic[A]) = new LogicProperties(
    name = "logic",
    parents = Seq(),
    ll = boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "¬(x∨y) = ¬x∧¬y" -> forAll { (x: A, y: A) => A.not(A.or(x, y)) ?== A.and(A.not(x), A.not(y)) },
    "¬(x∧y) = ¬¬(¬x∨¬y)" -> forAll { (x: A, y: A) => A.not(A.and(x, y)) ?== A.not(A.not(A.or(A.not(x), A.not(y)))) }
  )

  def deMorgan(implicit A: DeMorgan[A]) = new LogicProperties(
    name = "deMorgan",
    parents = Seq(logic),
    ll = boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "involutive" -> forAll { (x: A) => A.not(A.not(x)) ?== x }
  )

  class LogicProperties(
    val name: String,
    val parents: Seq[LogicProperties],
    val ll: LatticeProperties,
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("lattice" -> ll)
  }

}
