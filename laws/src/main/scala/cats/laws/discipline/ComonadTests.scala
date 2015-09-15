package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait ComonadTests[F[_]] extends CoflatMapTests[F] {

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  def laws: ComonadLaws[F]

  def comonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbitraryK[F].synthesize
    implicit val eqfa: Eq[F[A]] = EqK[F].synthesize
    implicit val eqffa: Eq[F[F[A]]] = EqK[F].synthesize
    implicit val eqfffa: Eq[F[F[F[A]]]] = EqK[F].synthesize
    implicit val eqfb: Eq[F[B]] = EqK[F].synthesize
    implicit val eqfc: Eq[F[C]] = EqK[F].synthesize

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
  def apply[F[_]: ArbitraryK: Comonad: EqK]: ComonadTests[F] =
    new ComonadTests[F] {
      def arbitraryK: ArbitraryK[F] = ArbitraryK[F]
      def eqK: EqK[F] = EqK[F]
      def laws: ComonadLaws[F] = ComonadLaws[F]
    }
}
