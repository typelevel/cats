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
    new RuleSet {
      def name: String = "rigidSelective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(selective[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "selective apply" -> forAll(laws.selectiveApply[A, B] _)
        )
    }
  }
}

object RigidSelectiveTests {
  def apply[F[_]: Selective]: RigidSelectiveTests[F] =
    new RigidSelectiveTests[F] {
      def laws: RigidSelectiveLaws[F] = RigidSelectiveLaws[F]
    }
}
