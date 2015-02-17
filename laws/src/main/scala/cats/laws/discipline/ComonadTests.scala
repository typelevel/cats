package cats.laws.discipline

import cats._
import cats.laws.{CoflatMapLaws, ComonadLaws}
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

  def coflatmap[C: Arbitrary](implicit F: CoflatMap[F], FC: Eq[F[C]]) = {
    val laws = CoflatMapLaws[F]
    new ComonadProperties(
      name = "coflatMap",
      parents = Nil,
      "associativity" -> forAll(laws.coflatMapAssociativity[A, B, C] _))
  }

  def comonad[C: Arbitrary](implicit F: Comonad[F], FC: Eq[F[C]], B: Eq[B]) = {
    val laws = ComonadLaws[F]
    new ComonadProperties(
      name = "comonad",
      parents = Seq(coflatmap[C]),
      "left identity" -> forAll(laws.comonadLeftIdentity[A] _),
      "right identity" -> forAll(laws.comonadRightIdentity[A, B] _))
  }

  class ComonadProperties(
    val name: String,
    val parents: Seq[ComonadProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
