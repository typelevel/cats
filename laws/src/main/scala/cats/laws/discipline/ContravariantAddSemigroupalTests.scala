package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}

trait ContravariantAddSemigroupalTests[F[_]] extends ContravariantTests[F] with InvariantAddSemigroupalTests[F] {
  def laws: ContravariantAddSemigroupalLaws[F]

  def contravariantAddSemigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[Either[A, Either[B, C]]]],
    EqFECC: Eq[F[Either[C, C]]]
  ): RuleSet =
    new RuleSet {
      val name = "contravariantAddSemigroupal"
      val parents = Seq(contravariant[A, B, C], invariantAddSemigroupal[A, B, C])
      val bases = Nil
      val props = Seq(
        "contramap sum distributivity" -> forAll(laws.contramapSumRightDistributivity[A, B, C] _)
      )
    }
}

object ContravariantAddSemigroupalTests {
  def apply[F[_]: ContravariantAddSemigroupal]: ContravariantAddSemigroupalTests[F] =
    new ContravariantAddSemigroupalTests[F] {
      def laws: ContravariantAddSemigroupalLaws[F] = ContravariantAddSemigroupalLaws[F]
    }
}
