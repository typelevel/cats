package cats.laws.discipline

import algebra.laws._
import cats._
import cats.functor._
import cats.laws.{ApplicativeLaws, ApplyLaws, FunctorLaws, InvariantLaws}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object FunctorTests {
  def apply[F[_]: ArbitraryK, A: Arbitrary](implicit eqfa: Eq[F[A]]): FunctorTests[F, A] =
    new FunctorTests[F, A] {
      def EqFA = eqfa
      def ArbA = implicitly[Arbitrary[A]]
      def ArbF = implicitly[ArbitraryK[F]]
    }
}

trait FunctorTests[F[_], A] extends Laws {

  implicit def EqFA: Eq[F[A]]
  def ArbF: ArbitraryK[F]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A](ArbA)

  def invariant[B: Arbitrary, C: Arbitrary](implicit F: Invariant[F], FC: Eq[F[C]]) = {
    val laws = InvariantLaws[F]
    new FunctorProperties(
      name = "functor",
      parents = Nil,
      "invariant identity" -> forAll(laws.invariantIdentity[A] _),
      "invariant composition" -> forAll(laws.invariantComposition[A, B, C] _))
  }

  def covariant[B: Arbitrary, C: Arbitrary](implicit F: Functor[F], FC: Eq[F[C]]) = {
    val laws = FunctorLaws[F]
    new FunctorProperties(
      name = "functor",
      parents = Seq(invariant[B, C]),
      "covariant identity" -> forAll(laws.covariantIdentity[A] _),
      "covariant composition" -> forAll(laws.covariantComposition[A, B, C] _))
  }

  def apply[B: Arbitrary, C: Arbitrary](implicit F: Apply[F], FC: Eq[F[C]]) = {
    implicit val ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit val ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]
    val laws = ApplyLaws[F]
    new FunctorProperties(
      name = "apply",
      parents = Seq(covariant[B, C]),
      "apply composition" -> forAll(laws.applyComposition[A, B, C] _))
  }

  def applicative[B: Arbitrary, C: Arbitrary](implicit F: Applicative[F], FC: Eq[F[C]]) = {
    implicit val ArbFAC: Arbitrary[F[A => C]] = ArbF.synthesize[A => C]
    implicit val ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit val ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]
    val laws = ApplicativeLaws[F]
    new FunctorProperties(
      name = "applicative",
      parents = Seq(apply[B, C]),
      "applicative identity" -> forAll(laws.applicativeIdentity[A] _),
      "applicative homomorphism" -> forAll(laws.applicativeHomomorphism[A, C] _),
      "applicative interchange" -> forAll(laws.applicativeInterchange[A, C] _),
      "applicative map" -> forAll(laws.applicativeMap[A, C] _),
      "applicative composition" -> forAll(laws.applicativeComposition[A, B, C] _))
    }

  class FunctorProperties(
    val name: String,
    val parents: Seq[FunctorProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
