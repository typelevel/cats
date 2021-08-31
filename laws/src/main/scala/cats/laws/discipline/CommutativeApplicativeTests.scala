package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeApplicativeTests[F[_]] extends CommutativeApplyTests[F] with ApplicativeTests[F] {

  def laws: CommutativeApplicativeLaws[F]

  def commutativeApplicative[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "commutative applicative"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], commutativeApply[A, B, C])
      def props: Seq[(String, Prop)] = Nil
    }
}

object CommutativeApplicativeTests {
  def apply[F[_]: CommutativeApplicative]: CommutativeApplicativeTests[F] =
    new CommutativeApplicativeTests[F] {
      def laws: CommutativeApplicativeLaws[F] = CommutativeApplicativeLaws[F]
    }
}
