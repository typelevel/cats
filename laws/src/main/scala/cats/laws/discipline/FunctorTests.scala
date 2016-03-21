package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait FunctorTests[F[_]] extends InvariantTests[F] {
  def laws: FunctorLaws[F]

  def functor[A: Arbitrary: Cogen, B: Arbitrary: Cogen, C: Arbitrary: Cogen](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "functor",
      parent = Some(invariant[A, B, C]),
      "covariant identity" -> forAll(laws.covariantIdentity[A] _),
      "covariant composition" -> forAll(laws.covariantComposition[A, B, C] _))
  }
}

object FunctorTests {
  def apply[F[_]: Functor]: FunctorTests[F] =
    new FunctorTests[F] { def laws: FunctorLaws[F] = FunctorLaws[F] }
}
