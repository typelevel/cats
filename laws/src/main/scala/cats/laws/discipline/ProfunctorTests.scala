package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import cats.arrow.Profunctor
import org.typelevel.discipline.Laws

trait ProfunctorTests[F[_, _]] extends Laws {
  def laws: ProfunctorLaws[F]

  def profunctor[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFCD: Arbitrary[F[C, D]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenD: Cogen[D],
    CogenE: Cogen[E],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAG: Eq[F[A, G]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "profunctor",
      parent = None,
      "profunctor identity" -> forAll(laws.profunctorIdentity[A, B] _),
      "profunctor composition" -> forAll(laws.profunctorComposition[A, B, C, D, E, G] _),
      "profunctor lmap identity" -> forAll(laws.profunctorLmapIdentity[A, B] _),
      "profunctor rmap identity" -> forAll(laws.profunctorRmapIdentity[A, B] _),
      "profunctor lmap composition" -> forAll(laws.profunctorLmapComposition[A, B, C, D] _),
      "profunctor rmap composition" -> forAll(laws.profunctorRmapComposition[A, D, C, B] _)
    )
}

object ProfunctorTests {
  def apply[F[_, _]: Profunctor]: ProfunctorTests[F] =
    new ProfunctorTests[F] { def laws: ProfunctorLaws[F] = ProfunctorLaws[F] }
}
