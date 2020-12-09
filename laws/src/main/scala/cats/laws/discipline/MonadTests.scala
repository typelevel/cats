package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.platform.Platform
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import Prop._

trait MonadTests[F[_]] extends SelectiveTests[F] with FlatMapTests[F] {
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
    iso: Isomorphisms[F]
  ): RuleSet = {
    implicit def ArbFAA: Arbitrary[F[Either[A, A]]] =
      Arbitrary(Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[A])),
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asRight[A]))))

    new RuleSet {
      def name: String = "monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(selective[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
          "monad right identity" -> forAll(laws.monadRightIdentity[A] _),
          "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[A, B] _)
        ) ++ (if (Platform.isJvm) Seq[(String, Prop)]("tailRecM stack safety" -> Prop.lzy(laws.tailRecMStackSafety))
              else Seq.empty)
    }
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
    iso: Isomorphisms[F]
  ): RuleSet = {
    implicit def ArbFAA: Arbitrary[F[Either[A, A]]] =
      Arbitrary(Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[A])),
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asRight[A]))))
    new RuleSet {
      def name: String = "monad (stack-unsafe)"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(selective[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
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
