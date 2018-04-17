package cats
package laws
package discipline

import cats.laws.ErrorControlLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ErrorControlTests[F[_], G[_], E] extends Laws {
  def laws: ErrorControlLaws[F, G, E]

  def errorControl[A]
  (implicit ArbA: Arbitrary[A],
   ArbF: Arbitrary[F[A]],
   ArbG: Arbitrary[G[A]],
   ArbE: Arbitrary[E],
   Arbf: Arbitrary[E => A],
   Arbf2: Arbitrary[A => E],
   Arbp: Arbitrary[A => Boolean],
   ArbfG: Arbitrary[E => G[A]],
   ArbfGa: Arbitrary[A => G[A]],
   EqFa: Eq[F[A]],
   EqGa: Eq[G[A]],
   EqFea: Eq[F[Either[E, A]]],
   EqGea: Eq[G[Either[E, A]]]
  ): RuleSet =
    new DefaultRuleSet(
      "errorControl",
      None,
      "raiseError controlError" -> forAll(laws.raiseErrorControlError[A] _),
      "raiseError intercept" -> forAll(laws.raiseErrorIntercept[A] _),
      "controlError pure is pure" -> forAll(laws.controlErrorPureIsPure[A] _),
      "G never has errors" -> forAll(laws.gNeverHasErrors[A] _),
      "raiseError trial" -> forAll(laws.raiseErrorTrial[A] _),
      "trial absolve cancel each oter" -> forAll(laws.trialAbsolve[A] _),
      "derive attempt" -> forAll(laws.deriveAttempt[A] _),
      "derive ensureOr" -> forAll(laws.deriveEnsureOr[A] _),
      "derive HandleError" -> forAll(laws.deriveHandleError[A] _),
      "monad homomorphism flatMap" -> forAll(laws.monadHomomorphismFlatMap[A] _),
      "monad homomorphism pure" -> forAll(laws.monadHomomorphismPure[A] _)
    )
}

object ErrorControlTests {
  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControlTests[F, G, E] =
    new ErrorControlTests[F, G, E] { val laws: ErrorControlLaws[F, G, E] = ErrorControlLaws[F, G, E] }
}
