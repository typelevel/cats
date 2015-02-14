package cats.laws.discipline

import algebra.laws._
import cats._
import cats.laws.{CoFlatMapLaws, ComonadLaws}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object ComonadTests {
  def apply[F[_]: ArbitraryK, A: Arbitrary, B: Arbitrary](implicit eqfa: Eq[F[A]]): ComonadTests[F, A, B] =
    new ComonadTests[F, A, B] {
      def EqFA = eqfa
      def ArbA = implicitly[Arbitrary[A]]
      def ArbB = implicitly[Arbitrary[B]]
      def ArbF = implicitly[ArbitraryK[F]]
    }
}

trait ComonadTests[F[_], A, B] extends Laws {

  implicit def EqFA: Eq[F[A]]
  def ArbF: ArbitraryK[F]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbB: Arbitrary[B]
  implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A](ArbA)
  implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B](ArbB)

  def coflatmap[C: Arbitrary](implicit F: CoFlatMap[F], FC: Eq[F[C]]) = {
    val laws = CoFlatMapLaws[F]
    new ComonadProperties(
      name = "coflatmap",
      parents = Nil,
      "associativity" -> forAll { (fa: F[A], f: F[A] => B, g: F[B] => C) =>
        val (lhs, rhs) = laws.coFlatMapAssociativity(fa, f, g)
        lhs ?== rhs
      }
    )
  }

  def comonad[C: Arbitrary](implicit F: Comonad[F], FC: Eq[F[C]], B: Eq[B]) = {
    val laws = ComonadLaws[F]
    new ComonadProperties(
      name = "comonad",
      parents = Seq(coflatmap[C]),
      "left identity" -> forAll { (fa: F[A]) =>
        val (lhs, rhs) = laws.comonadLeftIdentity(fa)
        lhs ?== rhs
      },
      "right identity" -> forAll { (fa: F[A], f: F[A] => B) =>
        val (lhs, rhs) = laws.comonadRightIdentity(fa, f)
        lhs ?== rhs
      }
    )
  }

  class ComonadProperties(
    val name: String,
    val parents: Seq[ComonadProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
