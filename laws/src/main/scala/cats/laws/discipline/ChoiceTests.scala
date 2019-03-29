package cats
package laws
package discipline

import cats.arrow.Choice
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ChoiceTests[F[_, _]] extends CategoryTests[F] {
  def laws: ChoiceLaws[F]

  def choice[A, B, C, D](implicit
                         ArbFAB: Arbitrary[F[A, B]],
                         ArbFAC: Arbitrary[F[A, C]],
                         ArbFBC: Arbitrary[F[B, C]],
                         ArbFCD: Arbitrary[F[C, D]],
                         EqFAB: Eq[F[A, B]],
                         EqFAD: Eq[F[A, D]],
                         EqFEitherABD: Eq[F[Either[A, B], D]]): RuleSet =
    new DefaultRuleSet(
      name = "choice",
      parent = Some(category[A, B, C, D]),
      "choice composition distributivity" -> forAll(laws.choiceCompositionDistributivity[A, B, C, D] _)
    )
}

object ChoiceTests {
  def apply[F[_, _]: Choice]: ChoiceTests[F] =
    new ChoiceTests[F] { def laws: ChoiceLaws[F] = ChoiceLaws[F] }
}
