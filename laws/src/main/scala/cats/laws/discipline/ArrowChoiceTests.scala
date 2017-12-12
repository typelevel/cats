package cats
package laws
package discipline

import cats.arrow.ArrowChoice
import cats.instances.function._
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait ArrowChoiceTests[F[_, _]] extends ArrowTests[F] with ChoiceTests[F] {
  def laws: ArrowChoiceLaws[F]

  def arrowChoice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFBC: Arbitrary[F[B, C]],
    ArbFAC: Arbitrary[F[A, C]],
    ArbFCD: Arbitrary[F[C, D]],
    ArbFDE: Arbitrary[F[D, E]],
    ArbFEG: Arbitrary[F[E, G]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAA: Eq[F[A, A]],
    EqFAB: Eq[F[A, B]],
    EqFAC: Eq[F[A, C]],
    EqFAD: Eq[F[A, D]],
    EqFAG: Eq[F[A, G]],
    EqFACB: Eq[F[(A, C), B]],
    EqFACBC: Eq[F[(A, C), (B, C)]],
    EqFACBD: Eq[F[(A, C), (B, D)]],
    EqFADCD: Eq[F[(A, D), (C, D)]],
    EqFADCG: Eq[F[(A, D), (C, G)]],
    EqFAEDE: Eq[F[(A, E), (D, E)]],
    EqFABC: Eq[F[A, (B, C)]],
    EqFEAED: Eq[F[(E, A), (E, D)]],
    EqFACDBCD: Eq[F[((A, C), D), (B, (C, D))]],
    EqFEitherABD: Eq[F[Either[A,B], D]],
    EqFEitherABC: Eq[F[Either[A,B], C]],
    EqFEitherABCD: Eq[F[Either[A, B], Either[C, D]]],
    LEqFEitherABC: Eq[F[Either[A, C], Either[B, C]]],
    REqFEitherABC: Eq[F[Either[C, A], Either[C, B]]],
    LEqFEitherABCD: Eq[F[Either[D, A], Either[D, C]]],
    REqFEitherABCD: Eq[F[Either[A, D], Either[C, D]]]
  ): RuleSet =
    new RuleSet {
      def name: String = "arrowChoice"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(
        arrow[A, B, C, D, E, G],
        choice[A, B, C, D]
      )
      def props: Seq[(String, Prop)] = Seq(
        "left and lift commute" -> forAll(laws.leftLiftCommute [A, B, C] _),
        "right and lift commute" -> forAll(laws.rightLiftCommute [A, B, C] _),
        "choose and lift commute" -> forAll(laws.chooseLiftCommute [A, B, C, D] _),
        "choice and lift commute" -> forAll(laws.choiceLiftCommute [A, B, C] _),
        "left and compose commute" -> forAll(laws.leftComposeCommute [A, B, C, D] _),
        "right and compose commute" -> forAll(laws.rightComposeCommute [A, B, C, D] _)
      )
    }
}
object ArrowChoiceTests {
  def apply[F[_, _]: ArrowChoice]: ArrowChoiceTests[F] =
    new ArrowChoiceTests[F] { def laws: ArrowChoiceLaws[F] = ArrowChoiceLaws[F] }
}
