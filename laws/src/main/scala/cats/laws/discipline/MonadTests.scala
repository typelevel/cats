package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.platform.Platform
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait MonadTests[F[_]] extends ApplicativeTests[F] with FlatMapTests[F] {
  def laws: MonadLaws[F]

  def monad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
                                                                  EqFInt: Eq[F[Int]],
                                                                  iso: Isomorphisms[F]): RuleSet =
    new RuleSet {
      def name: String = "monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
          "monad right identity" -> forAll(laws.monadRightIdentity[A] _),
          "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[A, B] _)
        ) ++ (if (Platform.isJvm) Seq[(String, Prop)]("tailRecM stack safety" -> Prop.lzy(laws.tailRecMStackSafety))
              else Seq.empty)
    }

  def stackUnsafeMonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
                                                                             EqFInt: Eq[F[Int]],
                                                                             iso: Isomorphisms[F]): RuleSet =
    new RuleSet {
      def name: String = "monad (stack-unsafe)"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
        "monad right identity" -> forAll(laws.monadRightIdentity[A] _),
        "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[A, B] _)
      )
    }
}

object MonadTests {
  def apply[F[_]: Monad]: MonadTests[F] =
    new MonadTests[F] {
      def laws: MonadLaws[F] = MonadLaws[F]
    }
}
