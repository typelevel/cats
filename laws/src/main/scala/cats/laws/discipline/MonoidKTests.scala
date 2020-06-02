package cats
package laws
package discipline

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

trait MonoidKTests[F[_]] extends SemigroupKTests[F] {
  def laws: MonoidKLaws[F]

  def monoidK[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], EqFA: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "monoidK",
      Some(semigroupK[A]),
      "monoidK left identity" -> forAll(laws.monoidKLeftIdentity[A] _),
      "monoidK right identity" -> forAll(laws.monoidKRightIdentity[A] _)
    )
}

object MonoidKTests {
  def apply[F[_]: MonoidK]: MonoidKTests[F] =
    new MonoidKTests[F] { def laws: MonoidKLaws[F] = MonoidKLaws[F] }
}
