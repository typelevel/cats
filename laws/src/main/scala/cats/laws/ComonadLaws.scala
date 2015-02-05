package cats.laws

import algebra.Eq
import algebra.laws._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

import cats._

object ComonadLaws {
  def apply[F[_]: ArbitraryK, A: Arbitrary, B: Arbitrary](implicit eqfa: Eq[F[A]]): ComonadLaws[F, A, B] =
    new ComonadLaws[F, A, B] {
      def EqFA = eqfa
      def ArbA = implicitly[Arbitrary[A]]
      def ArbB = implicitly[Arbitrary[B]]
      def ArbF = implicitly[ArbitraryK[F]]
    }
}

trait ComonadLaws[F[_], A, B] extends Laws {

  implicit def EqFA: Eq[F[A]]
  def ArbF: ArbitraryK[F]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbB: Arbitrary[B]
  implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A](ArbA)
  implicit def ArbFB: Arbitrary[F[B]] = ArbF.synthesize[B](ArbB)

  def coflatmap[C: Arbitrary](implicit F: CoFlatMap[F], FC: Eq[F[C]]) =
    new ComonadProperties(
      name = "coflatmap",
      parents = Nil,
      "associativity" -> forAll { (fa: F[A], f: F[A] => B, g: F[B] => C) =>
        F.coflatMap(F.coflatMap(fa)(f))(g) ?==
          F.coflatMap(fa)(x => g(F.coflatMap(x)(f)))
      }
    )

  def comonad[C: Arbitrary](implicit F: Comonad[F], FC: Eq[F[C]], B: Eq[B]) =
    new ComonadProperties(
      name = "comonad",
      parents = Seq(coflatmap[C]),
      "left identity" -> forAll { (fa: F[A]) =>
        F.coflatMap(fa)(F.extract) ?== fa
      },
      "right identity" -> forAll { (fa: F[A], f: F[A] => B) =>
        F.extract(F.coflatMap(fa)(f)) ?== f(fa)
      }
    )

  class ComonadProperties(
    val name: String,
    val parents: Seq[ComonadProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
