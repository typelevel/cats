package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import Prop._

trait SelectiveTests[F[_]] extends ApplicativeTests[F] {
  def laws: SelectiveLaws[F]

  def selective[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
    implicit val ArbFBool: Arbitrary[F[Boolean]] = arbFB[A, Boolean]
    implicit val ArbFUnit: Arbitrary[F[Unit]] = arbFB[A, Unit]
    new RuleSet {
      def name: String = "selective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "selective identity" -> forAll(laws.selectiveIdentity[A, B] _),
          "selective distributivity" -> forAll(laws.selectiveDistributivity[A, B] _),
          "selective associativity" -> forAll(laws.selectiveAssociativity[A, B, C] _),
          "selective branch consistency" -> forAll(laws.selectiveBranchConsistency[A, B, C] _),
          "selective ifS consistency" -> forAll(laws.selectiveIfSConsistency[A] _),
          "selective whenS consistency" -> forAll(laws.selectiveWhenSConsistency[A] _)
        )
    }
  }

  private def arbFB[A, B](implicit arbFA: Arbitrary[F[A]], arbB: Arbitrary[B]): Arbitrary[F[B]] =
    Arbitrary(for {
      fa <- arbFA.arbitrary
      b <- arbB.arbitrary
    } yield laws.F.as(fa, b))

  implicit private def arbFAtoBtoC[A, B, C](implicit
    arbFA: Arbitrary[F[A]],
    arbC: Arbitrary[C],
    cogenA: Cogen[A],
    cogenB: Cogen[B]
  ): Arbitrary[F[A => B => C]] =
    Arbitrary(for {
      fa <- arbFA.arbitrary
      f <- Gen.function1(Gen.function1(arbC.arbitrary)(cogenB))(cogenA)
    } yield laws.F.as(fa, f))

  implicit private def eqFUnit(implicit eqFInt: Eq[F[Int]]): Eq[F[Unit]] =
    Eq.by(laws.F.map(_)(_ => 0))
}

object SelectiveTests {
  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] {
      def laws: SelectiveLaws[F] = SelectiveLaws[F]
    }
}
