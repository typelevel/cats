package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._


trait TraverseTests[F[_]] extends FunctorTests[F] with FoldableTests[F] {
  def laws: TraverseLaws[F]

  def traverse[A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: Applicative, Y[_]: Applicative](implicit
    ArbFA: Arbitrary[F[A]],
    ArbXB: Arbitrary[X[B]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenM: Cogen[M],
    M: Monoid[M],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqM: Eq[M],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = {
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
        "traverse parallel composition" -> forAll(laws.traverseParallelComposition[A, B, X, Y] _),
        "traverse derive foldMap" -> forAll(laws.foldMapDerived[A, M] _)
      )
    }
  }
}

object TraverseTests {
  def apply[F[_]: Traverse]: TraverseTests[F] =
    new TraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
