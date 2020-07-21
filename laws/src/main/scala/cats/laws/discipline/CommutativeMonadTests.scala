package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeMonadTests[F[_]]
    extends MonadTests[F]
    with CommutativeFlatMapTests[F]
    with CommutativeApplicativeTests[F] {
  def laws: CommutativeMonadLaws[F]

  def commutativeMonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "commutative monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], commutativeFlatMap[A, B, C], commutativeApplicative[A, B, C])
      def props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeMonadTests {
  def apply[F[_]: CommutativeMonad]: CommutativeMonadTests[F] =
    new CommutativeMonadTests[F] {
      def laws: CommutativeMonadLaws[F] = CommutativeMonadLaws[F]
    }
}
