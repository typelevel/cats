package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.either._
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import Prop._

trait SelectiveTests[F[_]] extends ApplicativeTests[F] {
  def laws: SelectiveLaws[F]

  def selective[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
    iso: Isomorphisms[F]
  ): RuleSet = {
    // Derive implicits required after bincompat was locked in for 2.0

    implicit val ArbFCond: Arbitrary[F[Boolean]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield laws.F.as(fa, b))

    implicit val ArbFAA: Arbitrary[F[Either[A, A]]] = Arbitrary(
      Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[A])),
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asRight[A]))
      )
    )

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

    implicit val ArbFCAtoB: Arbitrary[F[Either[C, A => B]]] = Arbitrary(
      Gen.oneOf(
        ArbFC.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[A => B])),
        ArbFAtoB.arbitrary.map(faToB => laws.F.map(faToB)(_.asRight[C]))
      )
    )

    implicit val ArbCtoAtoB: Arbitrary[F[C => A => B]] = Arbitrary(
      for {
        fa <- ArbFA.arbitrary
        f <- Gen.function1(Gen.function1(Arbitrary.arbitrary[B])(CogenA))(CogenC)
      } yield laws.F.as(fa, f)
    )

    implicit val ArbFAtoC: Arbitrary[F[A => C]] =
      Arbitrary(for {
        fAToB <- ArbFAtoB.arbitrary
        fBToC <- ArbFBtoC.arbitrary
      } yield laws.F.map2(fAToB, fBToC)(_ andThen _))

    implicit val EqFB: Eq[F[B]] = Eq.by((fb: F[B]) => laws.F.map(fb)((null.asInstanceOf[A], _, null.asInstanceOf[C])))

    implicit val EqFUnit: Eq[F[Unit]] = {
      val a = Arbitrary.arbitrary[A].retryUntil(_ => true).sample.get
      Eq.by(laws.F.map(_)(_ => a))
    }

    new DefaultRuleSet(
      name = "applicative",
      parent = Some(apply[A, B, C]),
      "applicative identity" -> forAll(laws.applicativeIdentity[A] _),
      "applicative homomorphism" -> forAll(laws.applicativeHomomorphism[A, B] _),
      "applicative interchange" -> forAll(laws.applicativeInterchange[A, B] _),
      "applicative map" -> forAll(laws.applicativeMap[A, B] _),
      "applicative unit" -> forAll(laws.applicativeUnit[A] _),
      "ap consistent with product + map" -> forAll(laws.apProductConsistent[A, B] _),
      "monoidal left identity" -> forAll((fa: F[A]) => iso.leftIdentity(laws.monoidalLeftIdentity(fa))),
      "monoidal right identity" -> forAll((fa: F[A]) => iso.rightIdentity(laws.monoidalRightIdentity(fa))),
      "selective identity" -> forAll(laws.selectiveIdentity[A, B] _),
      "selective distributivity" -> forAll(laws.selectiveDistributivity[A, B] _),
      "select associativity" -> forAll(laws.selectAssociativity[A, B, C] _),
      "branch-select consistency" -> forAll(laws.branchSelectConsistency[A, B, C] _)
    )
  }
}

object SelectiveTests {
  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] { def laws: SelectiveLaws[F] = SelectiveLaws[F] }
}
