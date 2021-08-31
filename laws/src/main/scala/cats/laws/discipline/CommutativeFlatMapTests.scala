package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait CommutativeFlatMapTests[F[_]] extends FlatMapTests[F] with CommutativeApplyTests[F] {
  def laws: CommutativeFlatMapLaws[F]

  def commutativeFlatMap[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
  ): RuleSet =
    new RuleSet {
      def name: String = "commutative flatMap"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(flatMap[A, B, C], commutativeApply[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "flatmap commutativity" -> forAll(laws.flatmapCommutative[A, B, C] _)
        )
    }

}

object CommutativeFlatMapTests {
  def apply[F[_]: CommutativeFlatMap]: CommutativeFlatMapTests[F] =
    new CommutativeFlatMapTests[F] {
      def laws: CommutativeFlatMapLaws[F] = CommutativeFlatMapLaws[F]
    }
}
