package cats
package laws
package discipline

import cats.data.Xor
import cats.functor.ProChoice
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ProChoiceTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: ProChoiceLaws[F]

  def prochoice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAG: Eq[F[A, G]],
    EqFAEDE: Eq[F[A Xor E, D Xor E]],
    EqFEAED: Eq[F[E Xor A, E Xor D]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "prochoice",
      parent = Some(profunctor[A, B, C, D, E, G]),
      "prochoice left distributivity" -> forAll(laws.proChoiceLeftDistributivity[A, B, C, D, E] _),
      "prochoice right distributivity" -> forAll(laws.proChoiceRightDistributivity[A, B, C, D, E] _))
}

object ProChoiceTests {
  def apply[F[_, _]: ProChoice]: ProChoiceTests[F] =
    new ProChoiceTests[F] { def laws: ProChoiceLaws[F] = ProChoiceLaws[F] }
}
