package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait MonadTests[F[_]] extends ApplicativeTests[F] with FlatMapTests[F] {
  def laws: MonadLaws[F]

  def monad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
        "monad right identity" -> forAll(laws.monadRightIdentity[A] _),
        "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[A, B] _)
      )
    }
  }
}

object MonadTests {
  def apply[F[_]: Monad]: MonadTests[F] =
    new MonadTests[F] {
      def laws: MonadLaws[F] = MonadLaws[F]
    }
}
