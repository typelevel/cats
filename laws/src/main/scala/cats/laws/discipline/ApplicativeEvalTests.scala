package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait ApplicativeEvalTests[F[_]] extends ApplicativeTests[F] {
  def laws: ApplicativeEvalLaws[F]

  def applicativeEval[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "applicativeEval",
      parent = Some(applicative[A, B, C]),
      "eval consistent with pure" -> forAll(laws.evalEquivalenceWithPure[A] _),
      "eval consistent with pure mapped" -> forAll(laws.evalConsistentWithPureMapped[A, B] _)
    )
  }

  /**
   * In addition to the tests specified by [[applicativeEval]], it adds
   * an extra `ApplicativeError[F,Throwable]` restriction, which
   * enables extra laws.
   */
  def applicativeEvalWithError[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F],
    apErr: ApplicativeError[F, Throwable]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "applicativeEvalWithError",
      parent = Some(applicativeEval[A, B, C]),
      "eval captures exceptions" -> forAll(laws.evalCapturesExceptions[A] _)
    )
  }
}

object ApplicativeEvalTests {
  def apply[F[_]: ApplicativeEval]: ApplicativeEvalTests[F] =
    new ApplicativeEvalTests[F] {
      def laws: ApplicativeEvalLaws[F] = ApplicativeEvalLaws[F]
    }
}

