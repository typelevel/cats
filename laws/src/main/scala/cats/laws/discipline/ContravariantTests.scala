package cats
package laws
package discipline

import cats.functor.Contravariant
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait ContravariantTests[F[_]] extends InvariantTests[F] {
  def laws: ContravariantLaws[F]

  def contravariant[A: Arbitrary: Cogen, B: Arbitrary: Cogen, C: Arbitrary: Cogen](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "contravariant",
      parent = Some(invariant[A, B, C]),
      "contravariant identity" -> forAll(laws.contravariantIdentity[A] _),
      "contravariant composition" -> forAll(laws.contravariantComposition[A, B, C] _))
  }
}

object ContravariantTests {
  def apply[F[_]: Contravariant]: ContravariantTests[F] =
    new ContravariantTests[F] { def laws: ContravariantLaws[F] = ContravariantLaws[F] }
}
