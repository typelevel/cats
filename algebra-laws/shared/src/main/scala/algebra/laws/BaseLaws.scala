package algebra.laws

import cats.kernel._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}

@deprecated("No replacement", since = "2.7.0")
object BaseLaws {
  def apply[A: Eq: Arbitrary]: BaseLaws[A] = new BaseLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

@deprecated("No replacement", since = "2.7.0")
trait BaseLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  class BaseRuleSet(
    val name: String,
    val parent: Option[RuleSet],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet
      with HasOneParent
}
