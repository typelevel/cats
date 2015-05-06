package cats
package laws
package discipline

import cats.functor.Profunctor
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait ProfunctorTests[F[_, _]] extends Laws {
  def laws: ProfunctorLaws[F]

  def profunctor[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAG: Eq[F[A, G]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "profunctor",
      parent = None,
      "profunctor identity" -> forAll(laws.profunctorIdentity[A, B] _),
      "profunctor composition" -> forAll(laws.profunctorComposition[A, B, C, D, E, G] _))
}

object ProfunctorTests {
  def apply[F[_, _]: Profunctor]: ProfunctorTests[F] =
    new ProfunctorTests[F] { def laws: ProfunctorLaws[F] = ProfunctorLaws[F] }
}
