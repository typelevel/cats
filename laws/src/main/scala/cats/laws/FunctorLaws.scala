package cats.laws

import algebra.laws._

import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import cats._
import cats.functor._

object FunctorLaws {
  def apply[F[_]: ArbitraryK, A: Arbitrary](implicit eqfa: Eq[F[A]]): FunctorLaws[F, A] =
    new FunctorLaws[F, A] {
      def EqFA = eqfa
      def ArbA = implicitly[Arbitrary[A]]
      def ArbF = implicitly[ArbitraryK[F]]
    }
}

trait FunctorLaws[F[_], A] extends Laws {

  implicit def EqFA: Eq[F[A]]
  def ArbF: ArbitraryK[F]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A](ArbA)

  def invariant[B: Arbitrary, C: Arbitrary](implicit F: Invariant[F], FC: Eq[F[C]]) =
    new FunctorProperties(
      name = "functor",
      parents = Nil,
      "invariant identity" -> forAll { (fa: F[A]) =>
        F.imap(fa)(identity[A])(identity[A]) ?== fa
      },
      "invariant composition" -> forAll { (fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B) =>
        F.imap(F.imap(fa)(f1)(f2))(g1)(g2) ?== F.imap(fa)(f1 andThen g1)(g2 andThen f2)
      })

  def covariant[B: Arbitrary, C: Arbitrary](implicit F: Functor[F], FC: Eq[F[C]]) =
    new FunctorProperties(
      name = "functor",
      parents = Seq(invariant[B, C]),
      "covariant identity" -> forAll { (fa: F[A]) =>
        F.map(fa)(identity) ?== fa
      },
      "covariant composition" -> forAll { (fa: F[A], f: A => B, g: B => C) =>
        F.map(F.map(fa)(f))(g) ?== F.map(fa)(f andThen g)
      })

  def apply[B: Arbitrary, C: Arbitrary](implicit F: Apply[F], FC: Eq[F[C]]) = {
    implicit val ArbFAB: Arbitrary[F[A => B]] = ArbF.synthesize[A => B]
    implicit val ArbFBC: Arbitrary[F[B => C]] = ArbF.synthesize[B => C]
    new FunctorProperties(
      name = "apply",
      parents = Seq(covariant[B, C]),
      "apply composition" -> forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        try {
          val lhs = F.apply(F.apply(fa)(fab))(fbc)
          val rhs = F.apply(fa)(F.apply(fab)(F.map(fbc) { (bc: B => C) =>
            (ab: A => B) => ab andThen bc
          }))
          lhs ?== rhs
        } catch { case (e: StackOverflowError) =>
            e.printStackTrace
            throw e
        }
      })
  }

  def applicative[B: Arbitrary, C: Arbitrary](implicit F: Applicative[F], FC: Eq[F[C]]) = {
    implicit val ArbFAC: Arbitrary[F[A => C]] = ArbF.synthesize[A => C]
    new FunctorProperties(
      name = "applicative",
      parents = Seq(apply[B, C]),
      "applicative identity" -> forAll { (fa: F[A]) =>
        F.apply(fa)(F.pure((a: A) => a)) ?== fa
      },
      "applicative homomorphism" -> forAll { (a: A, f: A => C) =>
        F.apply(F.pure(a))(F.pure(f)) ?== F.pure(f(a))
      },
      "applicative interchange" -> forAll { (a: A, ff: F[A => C]) =>
        F.apply(F.pure(a))(ff) ?== F.apply(ff)(F.pure(f => f(a)))
      },
      "applicative map" -> forAll { (fa: F[A], f: A => C) =>
        F.map(fa)(f) ?== F.apply(fa)(F.pure(f))
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
