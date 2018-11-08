package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait DecideableTests[F[_]] extends ContravariantMonoidalTests[F] {
  def laws: DecideableLaws[F]

  def decideable[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
        val name = "decideable"
        val parents = Seq(contravariantMonoidal[A, B, C])
        val bases = Seq.empty
        val props = Seq(
          "decideable right absorption" ->
            forAll(laws.decideableDecideRightAbsorption[A] _),
          "decieable left unit" ->
            forAll(laws.decideableDecideLeftUnit[A] _),
          "decideable left distributivity" ->
            forAll(laws.decideableLeftDistributivity[A, B] _),
          "decideable right distributivity" ->
            forAll(laws.decideableRightDistributivity[A, B] _)
          )
      }
}
