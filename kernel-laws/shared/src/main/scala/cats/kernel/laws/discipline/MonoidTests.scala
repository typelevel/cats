package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonoidTests[A] extends SemigroupTests[A] {

  def laws: MonoidLaws[A]

  def monoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "monoid",
      Some(semigroup),
      "left identity" -> forAll(laws.leftIdentity _),
      "right identity" -> forAll(laws.rightIdentity _),
      "combine all" -> forAll(laws.combineAll _),
      "collect0" -> forAll(laws.collect0 _),
      "is id" -> forAll((a: A) => laws.isId(a, eqA)),
      "repeat0" -> forAll(laws.repeat0 _)
    )

}

object MonoidTests {
  def apply[A: Monoid]: MonoidTests[A] =
    new MonoidTests[A] { def laws: MonoidLaws[A] = MonoidLaws[A] }
}
