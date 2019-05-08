package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait PartialOrderTests[A] extends EqTests[A] {

  def laws: PartialOrderLaws[A]

  def partialOrder(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialOrder",
      Some(eqv),
      "transitivity" -> forAll(laws.transitivity _),
      "reflexivity lt" -> forAll(laws.reflexivityLt _),
      "reflexivity gt" -> forAll(laws.reflexivityGt _),
      "antisymmetry" -> forAll(laws.antisymmetry _),
      "gt" -> forAll(laws.gt _),
      "gteqv" -> forAll(laws.gteqv _),
      "lt" -> forAll(laws.lt _),
      "partialCompare" -> forAll(laws.partialCompare _),
      "pmax" -> forAll(laws.pmax _),
      "pmin" -> forAll(laws.pmin _)
    )

}

object PartialOrderTests {
  def apply[A: PartialOrder]: PartialOrderTests[A] =
    new PartialOrderTests[A] { def laws: PartialOrderLaws[A] = PartialOrderLaws[A] }
}
