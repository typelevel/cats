package cats
package laws
package discipline

import cats.laws.discipline.MonoidalTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait FlatMapTests[F[_]] extends ApplyTests[F] {
  def laws: FlatMapLaws[F]

  def flatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
      name = "flatMap",
      parent = Some(apply[A, B, C]),
      "flatMap associativity" -> forAll(laws.flatMapAssociativity[A, B, C] _),
      "flatMap consistent apply" -> forAll(laws.flatMapConsistentApply[A, B] _))
  }
}

object FlatMapTests {
  def apply[F[_]: FlatMap]: FlatMapTests[F] =
    new FlatMapTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
