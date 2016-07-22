package cats
package laws
package discipline

import org.scalacheck.{Prop, Arbitrary}
import Prop._

trait TraverseFilterTests[F[_]] extends TraverseTests[F] {
  def laws: TraverseFilterLaws[F]

  def traverseFilter[A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: Applicative, Y[_]: Applicative](implicit
    ArbFA: Arbitrary[F[A]],
    ArbXB: Arbitrary[X[B]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    ArbAXOB: Arbitrary[A => X[Option[B]]],
    ArbBYOC: Arbitrary[B => Y[Option[C]]],
    M: Monoid[M],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqM: Eq[M],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFA: Eq[X[F[A]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet = {
    implicit def EqXFBYFB : Eq[(X[F[B]], Y[F[B]])] = new Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        EqXFB.eqv(x._1, y._1) && EqYFB.eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "collect"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(traverse[A, B, C, M, X, Y])
      def props: Seq[(String, Prop)] = Seq(
        "traverseFilter identity" -> forAll(laws.traverseFilterIdentity[X, A] _),
        "traverseFilter composition" -> forAll(laws.traverseFilterComposition[A, B, C, X, Y] _)
      )
    }
  }
}

object TraverseFilterTests {
  def apply[F[_]: TraverseFilter]: TraverseFilterTests[F] =
    new TraverseFilterTests[F] { def laws: TraverseFilterLaws[F] = TraverseFilterLaws[F] }
}
