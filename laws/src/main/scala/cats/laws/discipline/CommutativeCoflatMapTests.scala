package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait CommutativeCoflatMapTests[F[_]] extends CoflatMapTests[F] {
  def laws: CommutativeCoflatMapLaws[F]

  def commutativeCoflatmap[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbFC: Arbitrary[F[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenFA: Cogen[F[A]],
    CogenFB: Cogen[F[B]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFA: Eq[F[A]],
    EqFFA: Eq[F[F[A]]],
    EqFFFA: Eq[F[F[F[A]]]],
    EqFFC: Eq[F[F[C]]],
    EqFFFC: Eq[F[F[F[C]]]],
    EqFInt: Eq[F[Int]]
  ): RuleSet = {
    new RuleSet {
      def name: String = "commutative coflatmap"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(coflatMap[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "coflatmap commutativity" -> forAll(laws.coflatmapCommutative[A, B, C] _)
      )
    }
  }

}

object CommutativeCoflatMapTests {
  def apply[F[_]:  CommutativeCoflatMap]: CommutativeCoflatMapTests[F] =
    new CommutativeCoflatMapTests[F] {
      def laws: CommutativeCoflatMapLaws[F] = CommutativeCoflatMapLaws[F]
    }
}
