package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait InvariantSemiringalTests[F[_]] extends InvariantChoosableTests[F] with InvariantMonoidalTests[F] {
  def laws: InvariantSemiringalLaws[F]

  def invariantSemiringal[A: Arbitrary, B: Arbitrary, C: Arbitrary](eqNothing: Eq[F[Nothing]])(implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFEABC: Eq[F[Either[A, Either[B, C]]]],
    EqFETABC: Eq[F[(Either[A, B], C)]],
    EqFABC: Eq[F[(A, (B, C))]],
    EqFABC2: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      implicit val nothingEq = eqNothing
      val name = "invariantSemiringal"
      val parents = Seq(invariantChoosable[A, B, C], invariantMonoidal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "invariant semiringal right absorption" -> forAll(laws.semiringalRightAbsorption[A] _)
      )
    }
}

object InvariantSemiringalTests {
  def apply[F[_]: InvariantSemiringal]: InvariantSemiringalTests[F] =
    new InvariantSemiringalTests[F] { def laws: InvariantSemiringalLaws[F] = InvariantSemiringalLaws[F] }
}
