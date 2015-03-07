package cats.laws
package discipline

import cats.{Eq, Functor}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait FunctorTests[F[_]] extends InvariantTests[F] {
  def laws: FunctorLaws[F]

  def functor[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new RuleSet {
      def name = "functor"
      def bases = Nil
      def parents = Seq(invariant[A, B, C])
      def props = Seq(
        "covariant identity" -> forAll(laws.covariantIdentity[A] _),
        "covariant composition" -> forAll(laws.covariantComposition[A, B, C] _)
      )
    }
  }
}

object FunctorTests {
  def apply[F[_]: Functor]: FunctorTests[F] =
    new FunctorTests[F] { def laws = FunctorLaws[F] }
}
