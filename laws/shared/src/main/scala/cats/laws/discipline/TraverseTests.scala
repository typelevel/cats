package cats
package laws
package discipline

import org.scalacheck.{Prop, Arbitrary}
import Prop._


trait TraverseTests[F[_]] extends FunctorTests[F] with FoldableTests[F] {
  def laws: TraverseLaws[F]

  def traverse[A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: Applicative, Y[_]: Applicative](implicit
    ArbF: ArbitraryK[F],
    ArbX: ArbitraryK[X],
    ArbY: ArbitraryK[Y],
    M: Monoid[M],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqM: Eq[M],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]
    implicit def ArbMB: Arbitrary[X[B]] = ArbX.synthesize[B]
    implicit def ArbNB: Arbitrary[Y[B]] = ArbY.synthesize[B]
    implicit def ArbNC: Arbitrary[Y[C]] = ArbY.synthesize[C]
    implicit def EqXFBYFB : Eq[(X[F[B]], Y[F[B]])] = new Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        EqXFB.eqv(x._1, y._1) && EqYFB.eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "traverse"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C], foldable[A, M])
      def props: Seq[(String, Prop)] = Seq(
        "traverse identity" -> forAll(laws.traverseIdentity[A, C] _),
        "traverse sequential composition" -> forAll(laws.traverseSequentialComposition[A, B, C, X, Y] _),
        "traverse parallel composition" -> forAll(laws.traverseParallelComposition[A, B, X, Y] _)
      )
    }
  }
}

object TraverseTests {
  def apply[F[_]: Traverse]: TraverseTests[F] =
    new TraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
