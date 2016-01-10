package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadFilterTests[F[_]] extends MonadTests[F] {
  def laws: MonadFilterLaws[F]

  def monadFilter[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "monadFilter",
      parent = Some(monad[A, B, C]),
      "monadFilter left empty" -> forAll(laws.monadFilterLeftEmpty[A, B] _),
      "monadFilter right empty" -> forAll(laws.monadFilterRightEmpty[A, B] _),
      "monadFilter consistency" -> forAll(laws.monadFilterConsistency[A, B] _))
  }
}

object MonadFilterTests {
  def apply[F[_]: MonadFilter]: MonadFilterTests[F] =
    new MonadFilterTests[F] {
      def laws: MonadFilterLaws[F] = MonadFilterLaws[F]
    }
}
