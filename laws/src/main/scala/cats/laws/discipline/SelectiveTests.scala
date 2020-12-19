package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

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
    // Derive implicits required after bincompat was locked in for 2.0

    implicit val ArbFCond: Arbitrary[F[Boolean]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield laws.F.as(fa, b))

    implicit val ArbFUnit: Arbitrary[F[Unit]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
    } yield laws.F.as(fa, ()))

    new RuleSet {
      def name: String = "selective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
        )
    }
  }
}

object SelectiveTests {
  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] {
      def laws: SelectiveLaws[F] = SelectiveLaws[F]
    }
}
