package cats
package laws
package discipline

import cats.arrow.CommutativeArrow
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait CommutativeArrowTests[F[_, _]] extends ArrowTests[F] {
  def laws: CommutativeArrowLaws[F]

  def commutativeArrow[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFAC: Arbitrary[F[A, C]],
    ArbFBC: Arbitrary[F[B, C]],
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
    EqFABC: Eq[F[A, (B, C)]],
    EqFACBC: Eq[F[(A, C), (B, C)]],
    EqFACBD: Eq[F[(A, C), (B, D)]],
    EqFADCD: Eq[F[(A, D), (C, D)]],
    EqFADCG: Eq[F[(A, D), (C, G)]],
    EqFDADB: Eq[F[(D, A), (D, B)]],
    EqFCADB: Eq[F[(C, A), (D, B)]],
    EqFACDBCD: Eq[F[((A, C), D), (B, (C, D))]],
    EqFACDBCD2: Eq[F[((A, C), D), ((B, C), D)]],
    EqFDCADCB: Eq[F[(D, (C, A)), (D, (C, B))]],
    EqFCAB: Eq[F[(C, A), B]]
  ): RuleSet =
    new DefaultRuleSet(name = "commutative arrow",
                       parent = Some(arrow[A, B, C, D, E, G]),
                       "arrow commutativity" -> forAll(laws.arrowCommutative[A, B, C, D] _)
    )
}

object CommutativeArrowTests {
  def apply[F[_, _]: CommutativeArrow]: CommutativeArrowTests[F] =
    new CommutativeArrowTests[F] { def laws: CommutativeArrowLaws[F] = CommutativeArrowLaws[F] }
}
