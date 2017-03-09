package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait MonadDeferTests[F[_]] extends ApplicativeEvalTests[F] with MonadTests[F] {
  def laws: MonadDeferLaws[F]

  def monadDefer[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
      def name: String = "monadDefer"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], applicativeEval[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "eval sequence consistent with pure.map" -> forAll(laws.evalSequenceConsistentWithPureMap[A, B] _),
        "eval repeats side effects" -> forAll(laws.evalRepeatsSideEffects[A, B] _),
        "defer sequence consistent with pure.flatMap" -> forAll(laws.deferSequenceConsistentWithPureFlatMap[A, B] _),
        "defer repeats side effects" -> forAll(laws.deferRepeatsSideEffects[A, B] _)
      )
    }
  }

  /**
   * In addition to the tests specified by [[monadDefer]], it adds
   * an extra `ApplicativeError[F,Throwable]` restriction, which
   * enables extra laws.
   */
  def monadDeferWithError[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
    iso: Isomorphisms[F],
    apErr: ApplicativeError[F, Throwable]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadDeferWithError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monadDefer[A, B, C], applicativeEvalWithError[A, B, C])
      def props: Seq[(String, Prop)] = Seq.empty
    }
  }
}

object MonadDeferTests {
  def apply[F[_]: MonadDefer]: MonadDeferTests[F] =
    new MonadDeferTests[F] {
      def laws: MonadDeferLaws[F] = MonadDeferLaws[F]
    }
}
