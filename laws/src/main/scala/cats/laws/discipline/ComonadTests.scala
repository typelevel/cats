package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait ComonadTests[F[_]] extends CoflatMapTests[F] {

  def laws: ComonadLaws[F]

  def comonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]],
    EqFFA: Eq[F[F[A]]],
    EqFFFA: Eq[F[F[F[A]]]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "comonad",
      parent = Some(coflatMap[A, B, C]),

      "extractCoflattenIdentity" -> forAll(laws.extractCoflattenIdentity[A] _),
      "mapCoflattenIdentity" -> forAll(laws.mapCoflattenIdentity[A] _),
      "coflattenThroughMap" -> forAll(laws.coflattenThroughMap[A] _),

      "coflattenCoherence" -> forAll(laws.coflattenCoherence[A, B] _),
      "coflatMapIdentity" -> forAll(laws.coflatMapIdentity[A, B] _),
      "mapCoflatMapCoherence" -> forAll(laws.mapCoflatMapCoherence[A, B] _),

      "comonad left identity" -> forAll(laws.comonadLeftIdentity[A] _),
      "comonad right identity" -> forAll(laws.comonadRightIdentity[A, B] _))
  }
}

object ComonadTests {
  def apply[F[_]: Comonad]: ComonadTests[F] =
    new ComonadTests[F] {
      def laws: ComonadLaws[F] = ComonadLaws[F]
    }
}
