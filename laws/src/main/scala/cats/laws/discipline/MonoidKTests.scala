package cats
package laws
package discipline

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

trait MonoidKTests[F[_]] extends SemigroupKTests[F] {
  def laws: MonoidKLaws[F]

  def monoidK[A: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new RuleSet {
      val name = "monoidK"
      val bases = Nil
      val parents = Seq(semigroupK[A])
      val props = Seq(
        "monoidK left identity" -> forAll(laws.monoidKLeftIdentity[A] _),
        "monoidK right identity" -> forAll(laws.monoidKRightIdentity[A] _)
      )
    }
  }
}

object MonoidKTests {
  def apply[F[_] : MonoidK]: MonoidKTests[F] =
    new MonoidKTests[F] { def laws: MonoidKLaws[F] = MonoidKLaws[F] }
}
