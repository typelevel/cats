package cats
package laws
package discipline

import cats.data.INothing
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait InvariantSemiringalTests[F[_]] extends InvariantAddMonoidalTests[F] with InvariantMonoidalTests[F] {
  def laws: InvariantSemiringalLaws[F]

  def invariantSemiringal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
                                                                    EqINothing: Eq[F[INothing]],
                                                                    iso: Isomorphisms[F]): RuleSet = new RuleSet {
    val name = "invariantSemiringal"
    val parents = Seq(invariantAddMonoidal[A, B, C], invariantMonoidal[A, B, C])
    val bases = Seq.empty
    val props = Seq(
      "invariant semiringal right absorption" -> forAll(laws.semiringalRightAbsorption[A] _),
      "invariant semiringal right distributivity" -> forAll(laws.semiringalRightDistributivity[A, B, C] _)
    )
  }
}

object InvariantSemiringalTests {
  def apply[F[_]: InvariantSemiringal]: InvariantSemiringalTests[F] =
    new InvariantSemiringalTests[F] { def laws: InvariantSemiringalLaws[F] = InvariantSemiringalLaws[F] }
}
