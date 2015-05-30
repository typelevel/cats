package cats
package laws
package discipline

import org.scalacheck.{Prop, Arbitrary}
import Prop._


trait TraverseTests[F[_]] extends FunctorTests[F] with FoldableTests[F] {
  def laws: TraverseLaws[F]

  def traverse[A: Arbitrary, B: Arbitrary, C: Arbitrary, M[_]: Applicative, N[_]: Applicative](implicit
    ArbF: ArbitraryK[F],
    ArbM: ArbitraryK[M],
    ArbN: ArbitraryK[N],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqMNFC: Eq[M[N[F[C]]]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbMB: Arbitrary[M[B]] = ArbM.synthesize[B]
    implicit def ArbNC: Arbitrary[N[C]] = ArbN.synthesize[C]
    new RuleSet {
      def name: String = "traverse"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C], foldable[A])
      def props: Seq[(String, Prop)] = Seq(
        "traverse identity" -> forAll(laws.traverseIdentity[A, C] _),
        "traverse composition" -> forAll(laws.traverseComposition[A, B, C, M, N] _)
      )
    }
  }
}

object TraverseTests {
  def apply[F[_]: Traverse]: TraverseTests[F] =
    new TraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}