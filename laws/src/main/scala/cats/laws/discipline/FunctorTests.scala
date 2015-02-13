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
      "invariant identity" -> forAll { (fa: F[A]) =>
        val (lhs, rhs) = laws.invariantIdentity(fa)
        lhs ?== rhs
      },
      "invariant composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
        val (lhs, rhs) = laws.invariantComposition(fa, f1, f2, g1, g2)
        lhs ?== rhs
      })
  }

  def covariant[B: Arbitrary, C: Arbitrary](implicit F: Functor[F], FC: Eq[F[C]]) = {
    val laws = FunctorLaws[F]
    new FunctorProperties(
      name = "functor",
      parents = Seq(invariant[B, C]),
      "covariant identity" -> forAll { (fa: F[A]) =>
        val (lhs, rhs) = laws.covariantIdentity(fa)
        lhs ?== rhs
      },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        val (lhs, rhs) = laws.covariantComposition(fa, f, g)
        lhs ?== rhs
      })
  }

  def apply[B: Arbitrary, C: Arbitrary](implicit F: Apply[F], FC: Eq[F[C]]) = {
    implicit val ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit val ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]
    val laws = ApplyLaws[F]
    new FunctorProperties(
      name = "apply",
      parents = Seq(covariant[B, C]),
      "apply composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        try {
          val (lhs, rhs) = laws.applyComposition(fa, fab, fbc)
          lhs ?== rhs
        } catch { case (e: StackOverflowError) =>
            e.printStackTrace
            throw e
        }
      })
  }

  def applicative[B: Arbitrary, C: Arbitrary](implicit F: Applicative[F], FC: Eq[F[C]]) = {
    implicit val ArbFAC: Arbitrary[F[A => C]] = ArbF.synthesize[A => C]
    implicit val ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit val ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]
    val laws = ApplicativeLaws[F]
    new FunctorProperties(
      name = "applicative",
      parents = Seq(apply[B, C]),
      "applicative identity" -> forAll { (fa: F[A]) =>
        val (lhs, rhs) = laws.applicativeIdentity(fa)
        lhs ?== rhs
      },
      "applicative homomorphism" -> forAll { (a: A, f: A => C) =>
        val (lhs, rhs) = laws.applicativeHomomorphism(a, f)
        lhs ?== rhs
      },
      "applicative interchange" -> forAll { (a: A, ff: F[A => C]) =>
        val (lhs, rhs) = laws.applicativeInterchange(a, ff)
        lhs ?== rhs
      },
      "applicative map" -> forAll { (fa: F[A], f: A => C) =>
        val (lhs, rhs) = laws.applicativeMap(fa, f)
        lhs ?== rhs
      },
      "applicative composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        val (lhs, rhs) = laws.applicativeComposition(fa, fab, fbc)
        lhs ?== rhs
      })
    }

  class FunctorProperties(
    val name: String,
    val parents: Seq[FunctorProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
