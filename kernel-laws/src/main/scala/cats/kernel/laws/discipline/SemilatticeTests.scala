package cats
package kernel
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}

trait SemilatticeTests[A] extends CommutativeSemigroupTests[A] with BandTests[A] {

  def laws: SemilatticeLaws[A]

  def semilattice(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "semilattice"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeSemigroup, band)
      val props: Seq[(String, Prop)] = Nil
    }

}

object SemilatticeTests {
  def apply[A: Semilattice]: SemilatticeTests[A] =
    new SemilatticeTests[A] { def laws: SemilatticeLaws[A] = SemilatticeLaws[A] }
}
