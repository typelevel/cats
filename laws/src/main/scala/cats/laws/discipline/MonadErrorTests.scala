package cats
package laws
package discipline

import cats.data.{Xor, XorT}
import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait MonadErrorTests[F[_], E] extends ApplicativeErrorTests[F, E] with MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  def monadError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "monadError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicativeError[A, B, C], monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadError left zero" -> forAll(laws.monadErrorLeftZero[A, B] _)
      )
    }
  }
}

object MonadErrorTests {
  def apply[F[_], E](implicit FE: MonadError[F, E]): MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
