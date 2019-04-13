package cats
package kernel
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait BandTests[A] extends SemigroupTests[A] {

  def laws: BandLaws[A]

  def band(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet("band", Some(semigroup), "idempotency" -> forAll(laws.idempotence _))

}

object BandTests {
  def apply[A: Band]: BandTests[A] =
    new BandTests[A] { def laws: BandLaws[A] = BandLaws[A] }
}
