package cats
package kernel
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait GroupLawTests[A] extends MonoidLawTests[A] {

  def laws: GroupLaws[A]

  def group(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "group",
      Some(monoid),
      "left inverse" -> forAll(laws.leftInverse _),
      "right inverse" -> forAll(laws.rightInverse _),
      "consistent inverse" -> forAll(laws.consistentInverse _))

}

object GroupLawTests {
  def apply[A: Group]: GroupLawTests[A] =
    new GroupLawTests[A] { def laws: GroupLaws[A] = GroupLaws[A] }
}
