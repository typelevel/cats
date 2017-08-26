package cats
package laws
package discipline


import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import cats.{Eq, ~>}
import org.typelevel.discipline.Laws

trait TFunctorTests[T[_[_], _]] extends Laws {
  def laws: TFunctorLaws[T]

  def tfunctor[F[_], G[_], H[_], A: Arbitrary](implicit
                                               ArbFA: Arbitrary[T[F, A]],
                                               ArbitraryG: Arbitrary[F[A]],
                                               ArbitraryH: Arbitrary[G[A]],
                                               ArbitraryI: Arbitrary[H[A]],
                                               ArbitraryFK: Arbitrary[F ~> G],
                                               ArbitraryFK2: Arbitrary[G ~> H],
                                               ArbitraryFK3: Arbitrary[G ~> F],
                                               ArbitraryFK4: Arbitrary[H ~> G],
                                               EqFA: Eq[T[F, A]],
                                               EqFC: Eq[T[H, A]]
                                              ): RuleSet = {
    new DefaultRuleSet(
      name = "TFunctor",
      parent = None,
      "covariant identity" -> forAll(laws.covariantIdentity[F, A] _),
      "covariant composition" -> forAll(laws.covariantComposition[F, G, H, A] _))
  }
}

object TFunctorTests {
  def apply[T[_[_], _]: TFunctor]: TFunctorTests[T] =
    new TFunctorTests[T] { def laws: TFunctorLaws[T] = TFunctorLaws[T] }
}
