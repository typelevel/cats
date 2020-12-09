package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait SelectiveTests[F[_]] extends ApplicativeTests[F] {
  def laws: SelectiveLaws[F]

  def selective[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbFAA: Arbitrary[F[Either[A, A]]],
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
    new RuleSet {
      def name: String = "selective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "selective identity" -> forAll(laws.selectiveIdentity[A, B] _)
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
