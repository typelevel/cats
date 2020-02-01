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
      "associative" -> forAll(laws.semigroupAssociative _),
      "repeat1" -> forAll(laws.repeat1 _),
      "repeat2" -> forAll(laws.repeat2 _),
      "combineAllOption" -> forAll(laws.combineAllOption _),
      "reverseReverses" -> forAll(laws.reverseReverses _),
      "reverseRepeat1" -> forAll(laws.reverseRepeat1 _),
      "reverseRepeat2" -> forAll(laws.reverseRepeat2 _),
      "reverseCombineAllOption" -> forAll(laws.reverseCombineAllOption _),
      "intercalateIntercalates" -> forAll(laws.intercalateIntercalates _),
      "intercalateRepeat1" -> forAll(laws.intercalateRepeat1 _),
      "intercalateRepeat2" -> forAll(laws.intercalateRepeat2 _),
      "intercalateCombineAllOption" -> forAll(laws.intercalateCombineAllOption _)
    )
}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}
