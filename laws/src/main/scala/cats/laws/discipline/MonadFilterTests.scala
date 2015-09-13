package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadFilterTests[F[_]] extends MonadTests[F] {
  def laws: MonadFilterLaws[F]

  def monadFilter[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbitraryK[F].synthesize
    implicit def ArbFB: Arbitrary[F[B]] = ArbitraryK[F].synthesize
    implicit def EqFB: Eq[F[B]] = EqK[F].synthesize

    new DefaultRuleSet(
      name = "monadFilter",
      parent = Some(monad[A, B, C]),
      "monadFilter left empty" -> forAll(laws.monadFilterLeftEmpty[A, B] _),
      "monadFilter right empty" -> forAll(laws.monadFilterRightEmpty[A, B] _))
  }
}

object MonadFilterTests {
  def apply[F[_]: MonadFilter: ArbitraryK: EqK]: MonadFilterTests[F] =
    new MonadFilterTests[F] {
      def arbitraryK: ArbitraryK[F] = implicitly
      def eqK: EqK[F] = implicitly
      def laws: MonadFilterLaws[F] = MonadFilterLaws[F]
    }
}
