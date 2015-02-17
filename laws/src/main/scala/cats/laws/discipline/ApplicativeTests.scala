package cats.laws
package discipline

import cats.{Applicative, Eq}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait ApplicativeTests[F[_]] extends ApplyTests[F] {
  def laws: ApplicativeLaws[F]

  def applicative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]

    new RuleSet {
      def name = "applicative"
      def bases = Nil
      def parents = Seq(apply[A, B, C])
      def props = Seq(
        "applicative identity" -> forAll(laws.applicativeIdentity[A] _),
        "applicative homomorphism" -> forAll(laws.applicativeHomomorphism[A, B] _),
        "applicative interchange" -> forAll(laws.applicativeInterchange[A, B] _),
        "applicative map" -> forAll(laws.applicativeMap[A, B] _)
      )
    }
  }
}

object ApplicativeTests {
  def apply[F[_]: Applicative]: ApplicativeTests[F] =
    new ApplicativeTests[F] { def laws = ApplicativeLaws[F] }
}
