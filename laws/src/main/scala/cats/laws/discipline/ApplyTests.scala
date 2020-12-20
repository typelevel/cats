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
  ): RuleSet = {
    // Derive implicits required after bincompat was locked in for 2.0

    implicit val ArbFCond: Arbitrary[F[Boolean]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield laws.F.as(fa, b))

    implicit val ArbFAB: Arbitrary[F[Either[A, B]]] = Arbitrary(
      Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[B])),
        ArbFB.arbitrary.map(fb => laws.F.map(fb)(_.asRight[A]))
      )
    )

    implicit val ArbFAC: Arbitrary[F[Either[A, C]]] = Arbitrary(
      Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[C])),
        ArbFC.arbitrary.map(fc => laws.F.map(fc)(_.asRight[A]))
      )
    )

    implicit val ArbFAtoC: Arbitrary[F[A => C]] =
      Arbitrary(for {
        fAToB <- ArbFAtoB.arbitrary
        fBToC <- ArbFBtoC.arbitrary
      } yield laws.F.map2(fAToB, fBToC)(_ andThen _))

    implicit val EqFB: Eq[F[B]] = Eq.by((fb: F[B]) => laws.F.map(fb)((null.asInstanceOf[A], _, null.asInstanceOf[C])))

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
        "branch/select consistency" -> forAll(laws.branchConsistency[A, B, C] _),
        "ifS/branch consistency" -> forAll(laws.ifSConsistency[A] _),
        "selectA consistent map2" -> forAll(laws.selectAConsistency[A, C] _)
      )
    }
  }
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
