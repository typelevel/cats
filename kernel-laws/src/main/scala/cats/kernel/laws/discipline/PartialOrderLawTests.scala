package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait PartialOrderLawTests[A] extends EqLawTests[A] {

  def laws: PartialOrderLaws[A]

  def partialOrder(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialOrder",
      Some(eqv),
      "transitivity" -> forAll(laws.transitivity _),
      "reflexitivity" -> forAll(laws.reflexitivity _),
      "antisymmetry" -> forAll(laws.antisymmetry _),
      "gt" -> forAll(laws.gt _),
      "gteqv" -> forAll(laws.gteqv _),
      "lt" -> forAll(laws.lt _),
      "partialCompare" -> forAll(laws.partialCompare _),
      "pmax" -> forAll(laws.pmax _),
      "pmin" -> forAll(laws.pmin _)
    )

}

object PartialOrderLawTests {
  def apply[A: PartialOrder]: PartialOrderLawTests[A] =
    new PartialOrderLawTests[A] { def laws: PartialOrderLaws[A] = PartialOrderLaws[A] }
}
