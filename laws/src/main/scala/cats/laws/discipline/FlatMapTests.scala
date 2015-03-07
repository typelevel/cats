package cats.laws
package discipline

import cats.{Eq, FlatMap}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait FlatMapTests[F[_]] extends ApplyTests[F] {
  def laws: FlatMapLaws[F]

  def flatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B]
    implicit def ArbFC: Arbitrary[F[C]] = ArbF.synthesize[C]
    implicit def ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]

    new RuleSet {
      def name = "flatMap"
      def bases = Nil
      def parents = Seq(apply[A, B, C])
      def props = Seq(
        "flatMap associativity" -> forAll(laws.flatMapAssociativity[A, B, C] _),
        "flatMap consistent apply" -> forAll(laws.flatMapConsistentApply[A, B] _)
      )
    }
  }
}

object FlatMapTests {
  def apply[F[_]: FlatMap]: FlatMapTests[F] =
    new FlatMapTests[F] { def laws = FlatMapLaws[F] }
}
