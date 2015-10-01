package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_], E] extends MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  implicit def arbitraryK: ArbitraryK[F]
  implicit def eqK: EqK[F]

  implicit def arbitraryE: Arbitrary[E]
  implicit def eqE: Eq[E]

  def monadError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq]: RuleSet = {
    implicit def ArbFEA: Arbitrary[F[A]] = arbitraryK.synthesize[A]
    implicit def ArbFEB: Arbitrary[F[B]] = arbitraryK.synthesize[B]
    implicit def EqFEA: Eq[F[A]] = eqK.synthesize[A]
    implicit def EqFEB: Eq[F[B]] = eqK.synthesize[B]

    new RuleSet {
      def name: String = "monadError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadError left zero" -> forAll(laws.monadErrorLeftZero[A, B] _),
        "monadError handle" -> forAll(laws.monadErrorHandle[A] _),
        "monadError pure" -> forAll(laws.monadErrorPure[A] _)
      )
    }
  }
}

object MonadErrorTests {
  def apply[F[_], E: Arbitrary: Eq](implicit FE: MonadError[F, E], ArbKF: ArbitraryK[F], EqKF: EqK[F]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def arbitraryE: Arbitrary[E] = implicitly[Arbitrary[E]]
      def arbitraryK: ArbitraryK[F] = ArbKF
      def eqE: Eq[E] = Eq[E]
      def eqK: EqK[F] = EqKF
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
