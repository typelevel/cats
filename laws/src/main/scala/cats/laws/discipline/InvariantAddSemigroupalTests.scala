package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}

trait InvariantAddSemigroupalTests[F[_]] extends InvariantTests[F] {
  def laws: InvariantAddSemigroupalLaws[F]

  def invariantAddSemigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[Either[A, Either[B, C]]]]
  ): RuleSet =
    new RuleSet {
      val name = "invariantAddSemigroupal"
      val parents = Seq(invariant[A, B, C])
      val bases = Nil
      val props = Seq(
        "invariant additive associativity" -> forAll(laws.sumAssociativity[A, B, C] _)
      )
    }
}

object InvariantAddSemigroupalTests {
  def apply[F[_]: InvariantAddSemigroupal]: InvariantAddSemigroupalTests[F] =
    new InvariantAddSemigroupalTests[F] { def laws: InvariantAddSemigroupalLaws[F] = InvariantAddSemigroupalLaws[F] }
}
