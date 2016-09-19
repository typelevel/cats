package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait AlternativeFlattenTests[F[_]] extends FunctorFilterTests[F] with AlternativeTests[F] {
  def laws: AlternativeFlattenLaws[F]

  def alternativeFlatten[A: Arbitrary, B: Arbitrary, C: Arbitrary, G1[_]: Traverse, G2[_]: Traverse](implicit
                                                        ArbFA: Arbitrary[F[A]],
                                                        ArbFB: Arbitrary[F[B]],
                                                        ArbFC: Arbitrary[F[C]],
                                                        ArbAOB: Arbitrary[A => Option[B]],
                                                        ArbBOC: Arbitrary[B => Option[C]],
                                                        ArbAG1B: Arbitrary[A => G1[B]],
                                                        ArbBG2C: Arbitrary[B => G2[C]],
                                                        ArbG1A: Arbitrary[G1[A]],
                                                        ArbFAB: Arbitrary[F[A => B]],
                                                        ArbFBC: Arbitrary[F[B => C]],
                                                        ArbAB: Arbitrary[A => C],
                                                        EqFA: Eq[F[A]],
                                                        EqFB: Eq[F[B]],
                                                        EqFC: Eq[F[C]],
                                                        EqFABC: Eq[F[(A, B, C)]],
                                                        iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name = "alternativeFlatten"
      def bases = Nil
      def parents = Seq(functorFilter[A, B, C], alternative[A, B, C])
      def props = Seq(
        "mapFlatten composition" -> forAll(laws.mapFlattenComposition[G1, G2, A, B, C] _),
        "traverseConsistency0" -> forAll(laws.traverseConsistency0[G1, A] _),
        "traverseConsistency1" -> forAll(laws.traverseConsistency1[G1, A] _))
    }
  }
}

object AlternativeFlattenTests {
  def apply[F[_]: AlternativeFlatten]: AlternativeFlattenTests[F] =
    new AlternativeFlattenTests[F] { def laws: AlternativeFlattenLaws[F] = AlternativeFlattenLaws[F] }
}
