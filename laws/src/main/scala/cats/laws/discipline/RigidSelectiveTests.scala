package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait RigidSelectiveTests[F[_]] extends SelectiveTests[F] {
  def laws: RigidSelectiveLaws[F]

  def rigidSelective[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
    implicit val ArbFAtoC: Arbitrary[F[A => C]] =
      Arbitrary(for {
        fAToB <- ArbFAtoB.arbitrary
        fBToC <- ArbFBtoC.arbitrary
      } yield laws.F.map2(fAToB, fBToC)(_ andThen _))

    new RuleSet {
      def name: String = "rigidSelective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(selective[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "selective apply" -> forAll(laws.selectiveApply[A, B] _),
          "selective select skip" -> forAll(laws.selectiveSelectSkip[A, B] _),
          "selective branch skip right" -> forAll(laws.selectiveBranchSkipRight[A, B, C] _),
          "selective branch skip left" -> forAll(laws.selectiveBranchSkipLeft[A, B, C] _),
          "selective ifS skip true" -> forAll(laws.selectiveIfSSkipTrue[A, B] _),
          "selective ifS skip false" -> forAll(laws.selectiveIfSSkipFalse[A, B] _),
          "selective whenS skip" -> forAll(laws.selectiveWhenSSkip[A] _)
        )
    }
  }
}

object RigidSelectiveTests {
  def apply[F[_]: RigidSelective]: RigidSelectiveTests[F] =
    new RigidSelectiveTests[F] {
      def laws: RigidSelectiveLaws[F] = RigidSelectiveLaws[F]
    }
}
