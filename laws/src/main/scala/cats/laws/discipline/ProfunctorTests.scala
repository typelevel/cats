package cats.laws
package discipline

import cats.Eq
import cats.functor.Profunctor
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ProfunctorTests[F[_, _]] extends SerializableTests {
  def laws: ProfunctorLaws[F]

  def profunctor[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
    ArbFAB: Arbitrary[F[A, B]],
    ArbFCD: Arbitrary[F[C, D]],
    EqFAB: Eq[F[A, B]],
    EqFAG: Eq[F[A, G]]
  ): RuleSet =
    new RuleSet {
      def name = "profunctor"
      def bases = Nil
      def parents = Seq(serializable[F[A, B]])
      def props = Seq(
        "profunctor identity" -> forAll(laws.profunctorIdentity[A, B] _),
        "profunctor composition" -> forAll(laws.profunctorComposition[A, B, C, D, E, G] _)
      )
    }
}

object ProfunctorTests {
  def apply[F[_, _]: Profunctor]: ProfunctorTests[F] =
    new ProfunctorTests[F] { def laws = ProfunctorLaws[F] }
}
