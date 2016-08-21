package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
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
    implicit def functorF: Functor[F] = laws.F
    implicit val EqFAB: Eq[F[(A, B)]] =
      ContravariantCartesian[Eq].composeFunctor[F].product(EqFA, EqFB)

    new DefaultRuleSet(
      name = "flatMap",
      parent = Some(apply[A, B, C]),
      "flatMap associativity" -> forAll(laws.flatMapAssociativity[A, B, C] _),
      "flatMap consistent apply" -> forAll(laws.flatMapConsistentApply[A, B] _),
      "followedBy consistent flatMap" -> forAll(laws.followedByConsistency[A, B] _),
      "mproduct consistent flatMap" -> forAll(laws.mproductConsistency[A, B] _),
      "tailRecM consistent flatMap" -> forAll(laws.tailRecMConsistentFlatMap[A] _))
  }
}

object FlatMapTests {
  def apply[F[_]: FlatMap]: FlatMapTests[F] =
    new FlatMapTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
