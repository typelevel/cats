package cats.kernel.laws

import cats.kernel._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}

object BaseLaws {
  def apply[A : Eq : Arbitrary] = new BaseLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait BaseLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  class BaseRuleSet(
    val name: String,
    val parent: Option[RuleSet],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent
}
