package cats
package laws
package discipline

import cats.ContravariantMonoidal
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait ContravariantMonoidalTests[F[_]] extends ContravariantSemigroupalTests[F] {
  def laws: ContravariantMonoidalLaws[F]

  def contravariantMonoidal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                                      arbFA: Arbitrary[F[A]],
                                                                      arbFB: Arbitrary[F[B]],
                                                                      arbFC: Arbitrary[F[C]],
                                                                      CogenA: Cogen[A],
                                                                      CogenB: Cogen[B],
                                                                      CogenC: Cogen[C],
                                                                      EqFA: Eq[F[A]],
                                                                      EqFB: Eq[F[B]],
                                                                      EqFC: Eq[F[C]],
                                                                      EqFABC: Eq[F[(A, B, C)]],
                                                                      iso: Isomorphisms[F]): RuleSet =
    new RuleSet {
      val name = "contravariantMonoidal"
      val parents = Seq(contravariantSemigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "contravariantMonoidal right unit" ->
          forAll(laws.contravariantMonoidalUnitRight[A] _),
        "contravariantMonoidal left unit" ->
          forAll(laws.contravariantMonoidalUnitLeft[A] _),
        "contravariantMonoidal contramap2 compatible contramap left" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C] _),
        "contravariantMonoidal contramap2 compatible contramap right" ->
          forAll(laws.contravariantMonoidalContramap2CompatibleContramapRight[A, B, C] _)
      )
    }
}

object ContravariantMonoidalTests {
  def apply[F[_]: ContravariantMonoidal]: ContravariantMonoidalTests[F] =
    new ContravariantMonoidalTests[F] { def laws: ContravariantMonoidalLaws[F] = ContravariantMonoidalLaws[F] }
}
