package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.either._
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import Prop._

trait SelectiveTests[F[_]] extends ApplicativeTests[F] {
  def laws: SelectiveLaws[F]

  def selective[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    // Derive implicits required after bincompat was locked in for 2.0

    implicit val ArbFCond: Arbitrary[F[Boolean]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield laws.F.as(fa, b))

    implicit val ArbFUnit: Arbitrary[F[Unit]] = Arbitrary(for {
      fa <- ArbFA.arbitrary
    } yield laws.F.as(fa, ()))

    implicit val ArbFAA: Arbitrary[F[Either[A, A]]] = Arbitrary(
      Gen.oneOf(
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asLeft[A])),
        ArbFA.arbitrary.map(fa => laws.F.map(fa)(_.asRight[A]))
      )
    )

    new RuleSet {
      def name: String = "selective"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "selective identity" -> forAll(laws.selectiveIdentity[A, B] _),
          "selective distributivity" -> forAll(laws.selectiveDistributivity[A, B] _),
          "selective whenS consistency" -> forAll(laws.selectiveWhenSConsistency[A] _)
        )
    }
  }

  implicit protected def derivedArbiraryFUnit(implicit eqFInt: Eq[F[Int]]): Eq[F[Unit]] =
    Eq.by(laws.F.map(_)(_ => 0))
}

object SelectiveTests {
  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] {
      def laws: SelectiveLaws[F] = SelectiveLaws[F]
    }
}
