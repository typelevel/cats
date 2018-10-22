package cats
package laws
package discipline

import cats.arrow.Arrow
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait ArrowTests[F[_, _]] extends CategoryTests[F] with StrongTests[F] {
  def laws: ArrowLaws[F]

  def arrow[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](
    implicit
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
    EqFACDBCD: Eq[F[((A, C), D), (B, (C, D))]]
  ): RuleSet =
    new RuleSet {
      def name: String = "arrow"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(
        category[A, B, C, D],
        strong[A, B, C, D, E, G]
      )
      def props: Seq[(String, Prop)] = Seq(
        "arrow identity" -> laws.arrowIdentity[A],
        "arrow composition" -> forAll(laws.arrowComposition[A, B, C] _),
        "arrow extension" -> forAll(laws.arrowExtension[A, B, C] _),
        "arrow functor" -> forAll(laws.arrowFunctor[A, B, C, D] _),
        "arrow exchange" -> forAll(laws.arrowExchange[A, B, C, D] _),
        "arrow unit" -> forAll(laws.arrowUnit[A, B, C] _),
        "arrow association" -> forAll(laws.arrowAssociation[A, B, C, D] _),
        "split consistent with andThen" -> forAll(laws.splitConsistentWithAndThen[A, B, C, D] _),
        "merge consistent with andThen" -> forAll(laws.mergeConsistentWithAndThen[A, B, C] _)
      )
    }
}

object ArrowTests {
  def apply[F[_, _]: Arrow]: ArrowTests[F] =
    new ArrowTests[F] { def laws: ArrowLaws[F] = ArrowLaws[F] }
}
