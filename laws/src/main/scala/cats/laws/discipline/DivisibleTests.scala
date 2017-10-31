package cats
package laws
package discipline

import cats.Divisible
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait DivisibleTests[F[_]] extends ContravariantTests[F] {
  def laws: DivisibleLaws[F]

  def divisible[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
    ): RuleSet = {
      new DefaultRuleSet(
        name = "divisible",
        parent = Some(contravariant[A, B, C]),
        "divisible right unit" -> forAll(laws.divisibleUnitRight[A] _),
        "divisible left unit" -> forAll(laws.divisibleUnitLeft[A] _),
        "divisible contramap2 compatible contramap left" -> forAll(laws.divisibleContramap2CompatibleContramapLeft[A, B, C] _),
        "divisible contramap2 compatible contramap right" -> forAll(laws.divisibleContramap2CompatibleContramapRight[A, B, C] _),
        "divisible contramap2 delta associates" -> forAll(laws.divisibleContramap2DiagonalAssociates[A] _)
        )
  }
}

object DivisibleTests {
  def apply[F[_]: Divisible]: DivisibleTests[F] =
    new DivisibleTests[F] { def laws: DivisibleLaws[F] = DivisibleLaws[F] }
}
