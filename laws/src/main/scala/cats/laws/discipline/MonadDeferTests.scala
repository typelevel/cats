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
        "delay equivalence with defer" -> forAll(laws.delayEquivalenceWithDefer[A, B] _),
        "delay repeats side effects" -> forAll(laws.delayRepeatsSideEffects[A, B] _),
        "defer repeats side effects" -> forAll(laws.deferRepeatsSideEffects[A, B] _)
      ) ++ (if (Platform.isJvm) Seq[(String, Prop)]("flatMap stack safety" -> Prop.lzy(laws.flatMapStackSafety)) else Seq.empty)
    }
  }
}

object MonadDeferTests {
  def apply[F[_]: MonadDefer]: MonadDeferTests[F] =
    new MonadDeferTests[F] {
      def laws: MonadDeferLaws[F] = MonadDeferLaws[F]
    }
}
