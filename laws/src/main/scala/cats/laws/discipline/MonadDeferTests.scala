package cats
package laws
package discipline

import catalysts.Platform
import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait MonadDeferTests[F[_]] extends MonadTests[F] {
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
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "delay equivalence with pure" -> forAll(laws.delayEquivalenceWithPure[A] _),
        "delay repeats side effects" -> forAll(laws.delayRepeatsSideEffects[A, B] _),
        "defer repeats side effects" -> forAll(laws.deferRepeatsSideEffects[A, B] _)
      ) ++ (if (Platform.isJvm) Seq[(String, Prop)]("flatMap stack safety" -> Prop.lzy(laws.flatMapStackSafety)) else Seq.empty)
    }
  }

  /**
   * In addition to the tests specified by [[MonadDefer]], it adds
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
      def parents: Seq[RuleSet] = Seq(monadDefer[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "delay captures exceptions" -> forAll(laws.delayCapturesExceptions[A] _),
        "defer captures exceptions" -> forAll(laws.deferCapturesExceptions[A] _)
      )
    }
  }
}

object MonadDeferTests {
  def apply[F[_]: MonadDefer]: MonadDeferTests[F] =
    new MonadDeferTests[F] {
      def laws: MonadDeferLaws[F] = MonadDeferLaws[F]
    }
}
