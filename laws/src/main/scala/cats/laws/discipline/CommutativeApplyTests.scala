package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait CommutativeApplyTests[F[_]] extends ApplyTests[F] {
  def laws: CommutativeApplyLaws[F]

  def commutativeApply[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "commutative apply"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(apply[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "apply commutativity" -> forAll(laws.applyCommutative[A, B, C] _)
      )
    }
  }

}

object CommutativeApplyTests {
  def apply[F[_]:  CommutativeApply]: CommutativeApplyTests[F] =
    new CommutativeApplyTests[F] {
      def laws: CommutativeApplyLaws[F] = CommutativeApplyLaws[F]
    }
}
