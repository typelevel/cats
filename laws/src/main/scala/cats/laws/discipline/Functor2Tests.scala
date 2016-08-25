package cats.laws.discipline

import cats.Eq
import cats.functor.Functor2
import cats.laws.Functor2Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait Functor2Tests[F[_, _]] extends Laws {
  def laws: Functor2Laws[F]

  def functor2[A, A2, A3, B, B2, B3](implicit
      ArbFAB: Arbitrary[F[A, B]],
      ArbA2: Arbitrary[A => A2],
      ArbA3: Arbitrary[A2 => A3],
      ArbB2: Arbitrary[B => B2],
      ArbB3: Arbitrary[B2 => B3],
      EqFAB: Eq[F[A, B]],
      EqFCZ: Eq[F[A3, B3]],
      EqFA3B: Eq[F[A3, B]],
      EqFAB3: Eq[F[A, B3]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "Functor2",
      parent = None,
      "Functor2 Identity" -> forAll(laws.functor2Identity[A, B] _),
      "Functor2 associativity" -> forAll(laws.functor2Composition[A, A2, A3, B, B2, B3] _),
      "Functor2 leftMap Identity" -> forAll(laws.functor2LeftMapIdentity[A, B] _),
      "Functor2 leftMap associativity" -> forAll(laws.functor2LeftMapComposition[A, B, A2, A3] _)
    )
  }
}

object Functor2Tests {
  def apply[F[_, _] : Functor2]: Functor2Tests[F] =
    new Functor2Tests[F] {
      def laws: Functor2Laws[F] = Functor2Laws[F]
    }
}
