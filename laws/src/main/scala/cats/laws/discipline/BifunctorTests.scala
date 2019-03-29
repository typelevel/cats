package cats.laws.discipline

import cats.{Bifunctor, Eq}
import cats.laws.BifunctorLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait BifunctorTests[F[_, _]] extends Laws {
  def laws: BifunctorLaws[F]

  def bifunctor[A, A2, A3, B, B2, B3](implicit
                                      ArbFAB: Arbitrary[F[A, B]],
                                      ArbA2: Arbitrary[A => A2],
                                      ArbA3: Arbitrary[A2 => A3],
                                      ArbB2: Arbitrary[B => B2],
                                      ArbB3: Arbitrary[B2 => B3],
                                      EqFAB: Eq[F[A, B]],
                                      EqFCZ: Eq[F[A3, B3]],
                                      EqFA3B: Eq[F[A3, B]],
                                      EqFAB3: Eq[F[A, B3]]): RuleSet =
    new DefaultRuleSet(
      name = "Bifunctor",
      parent = None,
      "Bifunctor Identity" -> forAll(laws.bifunctorIdentity[A, B] _),
      "Bifunctor associativity" -> forAll(laws.bifunctorComposition[A, A2, A3, B, B2, B3] _),
      "Bifunctor leftMap Identity" -> forAll(laws.bifunctorLeftMapIdentity[A, B] _),
      "Bifunctor leftMap associativity" -> forAll(laws.bifunctorLeftMapComposition[A, B, A2, A3] _)
    )
}

object BifunctorTests {
  def apply[F[_, _]: Bifunctor]: BifunctorTests[F] =
    new BifunctorTests[F] {
      def laws: BifunctorLaws[F] = BifunctorLaws[F]
    }
}
