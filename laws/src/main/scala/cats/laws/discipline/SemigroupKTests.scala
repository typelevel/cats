package cats
package laws
package discipline

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait SemigroupKTests[F[_]] extends Laws {
  def laws: SemigroupKLaws[F]

  def semigroupK[A: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]]
  ): RuleSet = {
    new RuleSet {
      val name = "semigroupK"
      val bases = Nil
      val parents = Nil
      val props = Seq(
        "semigroupK associative" -> forAll(laws.semigroupKAssociative[A] _)
      )
    }
  }
}

object SemigroupKTests {
  def apply[F[_]: SemigroupK]: SemigroupKTests[F] =
    new SemigroupKTests[F] { def laws: SemigroupKLaws[F] = SemigroupKLaws[F] }
}
