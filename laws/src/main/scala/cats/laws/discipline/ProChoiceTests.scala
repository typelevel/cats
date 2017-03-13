package cats
package laws
package discipline

import cats.arrow.ProChoice
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait ProChoiceTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: ProChoiceLaws[F]

  def prochoice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFCD: Arbitrary[F[C, D]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    CogenG: Cogen[G],
    EqFAB: Eq[F[A, B]],
    EqFAG: Eq[F[A, G]],
    EqFAD: Eq[F[A, D]],
    EqFAEDE: Eq[F[Either[A, E], Either[D, E]]],
    EqFEAED: Eq[F[Either[E, A], Either[E, D]]]
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
