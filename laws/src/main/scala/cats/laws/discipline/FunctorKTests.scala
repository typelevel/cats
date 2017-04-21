package cats
package laws
package discipline

import cats.arrow.FunctionK
import org.scalacheck.{Arbitrary, Prop}
import Prop._
import org.typelevel.discipline.Laws

trait FunctorKTests[F[_[_]]] extends Laws {
  def laws: FunctorKLaws[F]

  def functorK[G[_], H[_], I[_], A: Arbitrary](implicit
    ArbFA: Arbitrary[F[G]],
    ArbitraryG: Arbitrary[G[A]],
    ArbitraryH: Arbitrary[H[A]],
    ArbitraryI: Arbitrary[I[A]],
    ArbitraryFK: Arbitrary[FunctionK[G, H]],
    ArbitraryFK2: Arbitrary[FunctionK[H, I]],
    EqFA: Eq[F[G]],
    EqFC: Eq[F[I]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "functorK",
      parent = None,
      "covariant identity" -> forAll(laws.covariantIdentity[G] _),
      "covariant composition" -> forAll(laws.covariantComposition[G, H, I] _))
  }
}

object FunctorKTests {
  def apply[F[_[_]]: FunctorK]: FunctorKTests[F] =
    new FunctorKTests[F] { def laws: FunctorKLaws[F] = FunctorKLaws[F] }
}
