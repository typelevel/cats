package cats
package kernel
package laws
package discipline

import cats.kernel.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def semigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "semigroup",
      None,
      "semigroup associative" -> forAll(laws.semigroupAssociative _),
      "semigroup repeat1" -> forAll(laws.repeat1 _),
      "semigroup repeat2" -> forAll(laws.repeat2 _),
      "semigroup combineAllOption" -> forAll(laws.combineAllOption _))
}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}
