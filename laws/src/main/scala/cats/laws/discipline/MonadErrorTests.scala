package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_, _], E] extends MonadTests[F[E, ?]] {
  def laws: MonadErrorLaws[F, E]

  def monadError[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit
    ArbF: ArbitraryK[F[E, ?]],
    EqFA: Eq[F[E, A]],
    EqFB: Eq[F[E, B]],
    EqFC: Eq[F[E, C]],
    ArbE: Arbitrary[E]
  ): RuleSet = {
    implicit def ArbFEA: Arbitrary[F[E, A]] = ArbF.synthesize[A]
    implicit def ArbFEB: Arbitrary[F[E, B]] = ArbF.synthesize[B]

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
  def apply[F[_, _], E](implicit FE: MonadError[F, E]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] { def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E] }
}
