package cats
package laws
package discipline

import cats.data.{Xor, XorT}
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait ApplicativeErrorTests[F[_], E] extends ApplicativeTests[F] {
  def laws: ApplicativeErrorLaws[F, E]

  def applicativeError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbE: Arbitrary[E],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqE: Eq[E],
    EqFEitherEU: Eq[F[Xor[E, Unit]]],
    EqFEitherEA: Eq[F[Xor[E, A]]],
    EqEitherTFEA: Eq[XorT[F, E, A]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "applicativeError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "applicativeError handleWith" -> forAll(laws.applicativeErrorHandleWith[A] _),
        "applicativeError handle" -> forAll(laws.applicativeErrorHandle[A] _),
        "applicativeError handleErrorWith pure" -> forAll(laws.handleErrorWithPure[A] _),
        "applicativeError handleError pure" -> forAll(laws.handleErrorPure[A] _),
        "applicativeError raiseError attempt" -> forAll(laws.raiseErrorAttempt _),
        "applicativeError pure attempt" -> forAll(laws.pureAttempt[A] _),
        "applicativeError handleErrorWith consistent with recoverWith" -> forAll(laws.handleErrorWithConsistentWithRecoverWith[A] _),
        "applicativeError handleError consistent with recover" -> forAll(laws.handleErrorConsistentWithRecover[A] _),
        "applicativeError recover consistent with recoverWith" -> forAll(laws.recoverConsistentWithRecoverWith[A] _),
        "applicativeError attempt consistent with attemptT" -> forAll(laws.attemptConsistentWithAttemptT[A] _)
      )
    }
  }
}

object ApplicativeErrorTests {
  def apply[F[_], E](implicit FE: ApplicativeError[F, E]): ApplicativeErrorTests[F, E] =
    new ApplicativeErrorTests[F, E] {
      def laws: ApplicativeErrorLaws[F, E] = ApplicativeErrorLaws[F, E]
    }
}
