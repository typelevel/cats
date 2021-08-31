package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait ApplyTests[F[_]] extends FunctorTests[F] with SemigroupalTests[F] {
  def laws: ApplyLaws[F]

  def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name = "apply"
      val parents = Seq(functor[A, B, C], semigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "apply composition" -> forAll(laws.applyComposition[A, B, C] _),
        "map2/product-map consistency" -> forAll(laws.map2ProductConsistency[A, B, C] _),
        "map2/map2Eval consistency" -> forAll(laws.map2EvalConsistency[A, B, C] _),
        "productR consistent map2" -> forAll(laws.productRConsistency[A, C] _),
        "productL consistent map2" -> forAll(laws.productLConsistency[A, C] _)
      )
    }
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
