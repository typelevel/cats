package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeComonadTests[F[_]] extends ComonadTests[F] with CommutativeCoflatMapTests[F] {
  def laws: CommutativeComonadLaws[F]

  def commutativeComonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "commutative comonad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(comonad[A, B, C], commutativeCoflatmap[A, B, C] )
      def props: Seq[(String, Prop)] = Nil
    }
  }
}

object CommutativeComonadTests {
  def apply[F[_]:  CommutativeComonad]: CommutativeComonadTests[F] =
    new CommutativeComonadTests[F] {
      def laws: CommutativeComonadLaws[F] = CommutativeComonadLaws[F]
    }
}
