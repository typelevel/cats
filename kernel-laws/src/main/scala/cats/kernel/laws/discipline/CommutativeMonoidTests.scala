package cats
package kernel
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}

trait CommutativeMonoidTests[A] extends CommutativeSemigroupTests[A] with MonoidTests[A] {
  def laws: CommutativeMonoidLaws[A]

  def commutativeMonoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "commutativeMonoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeSemigroup, monoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeMonoidTests {
  def apply[A: CommutativeMonoid]: CommutativeMonoidTests[A] =
    new CommutativeMonoidTests[A] { def laws: CommutativeMonoidLaws[A] = CommutativeMonoidLaws[A] }
}
