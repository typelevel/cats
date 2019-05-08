package cats
package kernel
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}

trait BoundedSemilatticeTests[A] extends CommutativeMonoidTests[A] with SemilatticeTests[A] {

  def laws: BoundedSemilatticeLaws[A]

  def boundedSemilattice(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "boundedSemilattice"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeMonoid, semilattice)
      val props: Seq[(String, Prop)] = Nil
    }

}

object BoundedSemilatticeTests {
  def apply[A: BoundedSemilattice]: BoundedSemilatticeTests[A] =
    new BoundedSemilatticeTests[A] { def laws: BoundedSemilatticeLaws[A] = BoundedSemilatticeLaws[A] }
}
