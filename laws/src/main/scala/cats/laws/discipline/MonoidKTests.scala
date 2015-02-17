package cats
package laws
package discipline

import algebra.laws._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

object MonoidKTests {
  def apply[F[_]: ArbitraryK, A: Arbitrary](implicit eqfa: Eq[F[A]]): MonoidKTests[F, A] =
    new MonoidKTests[F, A] {
      def EqFA = eqfa
      def ArbA = implicitly[Arbitrary[A]]
      def ArbF = implicitly[ArbitraryK[F]]
    }
}

trait MonoidKTests[F[_], A] extends SemigroupKTests[F, A] {
  def identity(implicit A: MonoidK[F]) = {
    val laws = MonoidKLaws[F]
    new SemigroupKProperties(
      name = "monoidK",
      parents = Seq(associative),
      "left identity" -> forAll(laws.leftIdentity[A] _),
      "right identity" -> forAll(laws.rightIdentity[A] _)
    )
  }
}
