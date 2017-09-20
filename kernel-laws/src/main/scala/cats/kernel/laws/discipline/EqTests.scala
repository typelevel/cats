package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait EqTests[A] extends Laws {
  def laws: EqLaws[A]

  def eqv(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "eq",
      None,
      "eq reflexitivity" -> forAll(laws.reflexitivityEq _),
      "eq symmetry" -> forAll(laws.symmetryEq _),
      "eq antisymmetry" -> forAll(laws.antiSymmetryEq _),
      "eq transitivity" -> forAll(laws.transitivityEq _))
}

object EqTests {
  def apply[A: Eq]: EqTests[A] =
    new EqTests[A] { def laws: EqLaws[A] = EqLaws[A] }
}

