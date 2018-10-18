package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import cats.kernel.CommutativeMonoid

trait UnorderedTraverseTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: UnorderedTraverseLaws[F]

  def unorderedTraverse[A: Arbitrary, B: Arbitrary, C: Arbitrary, X[_]: CommutativeApplicative, Y[_]: CommutativeApplicative](
    implicit ArbFA: Arbitrary[F[A]],
    ArbFXB: Arbitrary[F[X[B]]],
    ArbXB: Arbitrary[X[B]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    Ca: CommutativeMonoid[A],
    Cb: CommutativeMonoid[B],
    EqA: Eq[A],
    EqB: Eq[B],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet = {
    implicit def EqXFBYFB: Eq[(X[F[B]], Y[F[B]])] = new Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        EqXFB.eqv(x._1, y._1) && EqYFB.eqv(x._2, y._2)
    }
    new DefaultRuleSet(
      name = "unorderedTraverse",
      parent = Some(unorderedFoldable[A, B]),
      "unordered traverse sequential composition" -> forAll(
        laws.unorderedTraverseSequentialComposition[A, B, C, X, Y] _
      ),
      "unordered traverse parallel composition" -> forAll(laws.unorderedTraverseParallelComposition[A, B, X, Y] _),
      "unordered traverse consistent with sequence" -> forAll(laws.unorderedSequenceConsistent[B, X] _)
    )
  }
}

object UnorderedTraverseTests {
  def apply[F[_]: UnorderedTraverse]: UnorderedTraverseTests[F] =
    new UnorderedTraverseTests[F] { def laws: UnorderedTraverseLaws[F] = UnorderedTraverseLaws[F] }
}
