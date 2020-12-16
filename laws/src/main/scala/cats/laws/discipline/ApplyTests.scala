package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
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
        "productL consistent map2" -> forAll(laws.productLConsistency[A, C] _),
        "selectA consistent map2" -> forAll(laws.selectAConsistency[A, C] _)
      )
    }

  // Derived implicits to preserve bincompat
  implicit protected def derivedArbitraryEither[A, B](implicit
    arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]]
  ): Arbitrary[F[Either[A, B]]] = {
    Arbitrary(
      Gen.oneOf(arbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[B])),
                arbFB.arbitrary.map(fb => laws.F.map(fb)(_.asRight[A]))
      )
    )
  }

  implicit protected def derivedArbitraryFunctionComposition[A, B, C](implicit
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]]
  ): Arbitrary[F[A => C]] =
    Arbitrary(for {
      fAToB <- arbFAtoB.arbitrary
      fBToC <- arbFBtoC.arbitrary
    } yield laws.F.map2(fAToB, fBToC)(_ andThen _))
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
