package cats
package kernel
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait CommutativeSemigroupTests[A] extends SemigroupTests[A] {

  def laws: CommutativeSemigroupLaws[A]

  def commutativeSemigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet("commutativeSemigroup", Some(semigroup), "commutative" -> forAll(laws.commutative _))

}

object CommutativeSemigroupTests {
  def apply[A: CommutativeSemigroup]: CommutativeSemigroupTests[A] =
    new CommutativeSemigroupTests[A] { def laws: CommutativeSemigroupLaws[A] = CommutativeSemigroupLaws[A] }
}
