package cats
package kernel
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}

trait CommutativeGroupTests[A] extends CommutativeMonoidTests[A] with GroupTests[A] {
  def laws: CommutativeGroupLaws[A]

  def commutativeGroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "commutativeGroup"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeMonoid, group)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeGroupTests {
  def apply[A: CommutativeGroup]: CommutativeGroupTests[A] =
    new CommutativeGroupTests[A] { def laws: CommutativeGroupLaws[A] = CommutativeGroupLaws[A] }
}
