package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait OrderTests[A] extends PartialOrderTests[A] {

  def laws: OrderLaws[A]

  def order(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "order",
      Some(partialOrder),
      "totality" -> forAll(laws.totality _),
      "compare" -> forAll(laws.compare _),
      "max" -> forAll(laws.max _),
      "min" -> forAll(laws.min _)
    )

}

object OrderTests {
  def apply[A: Order]: OrderTests[A] =
    new OrderTests[A] { def laws: OrderLaws[A] = OrderLaws[A] }
}
