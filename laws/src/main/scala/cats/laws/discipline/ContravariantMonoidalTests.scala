package cats
package laws
package discipline

import cats.ContravariantMonoidal
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait ContravariantMonoidalTests[F[_]] extends ContravariantTests[F] {
  def laws: ContravariantMonoidalLaws[F]

  def contravariantMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    arbFA: Arbitrary[F[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
    ): RuleSet = {
      new DefaultRuleSet(
        name = "contravariantMonoidal",
        parent = Some(contravariant[A, B, C]),
        "contravariantMonoidal right unit" ->
          forAll(laws.contravariantMonoidalUnitRight[A] _),
        "contravariantMonoidal left unit" ->
          forAll(laws.contravariantMonoidalUnitLeft[A] _),
        "contravariantMonoidal contramap2 compatible contramap left" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C] _),
        "contravariantMonoidal contramap2 compatible contramap right" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapRight[A, B, C] _),
        "contravariantMonoidal contramap2 delta associates" ->
          forAll(laws.contravariantMonoidalContramap2DiagonalAssociates[A] _)
        )
  }
}

object ContravariantMonoidalTests {
  def apply[F[_]: ContravariantMonoidal]: ContravariantMonoidalTests[F] =
    new ContravariantMonoidalTests[F] { def laws: ContravariantMonoidalLaws[F] = ContravariantMonoidalLaws[F] }
}
