package cats.laws
package discipline

import cats.{Apply, Eq}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ApplyTests[F[_]] extends FunctorTests[F] {
  def laws: ApplyLaws[F]

  def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit def ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]

    new RuleSet {
      def name = "apply"
      def bases = Nil
      def parents = Seq(functor[A, B, C])
      def props = Seq(
        "apply composition" -> forAll(laws.applyComposition[A, B, C] _)
      )
    }
  }
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws = ApplyLaws[F] }
}
