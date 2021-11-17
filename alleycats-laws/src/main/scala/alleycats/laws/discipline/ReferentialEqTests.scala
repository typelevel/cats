package alleycats.laws.discipline

import cats.kernel.Eq
import cats.kernel.laws.EqLaws
import cats.kernel.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ReferentialEqTests[A] extends Laws {
  def laws: EqLaws[A]

  def eqv(implicit arbA: Arbitrary[A]): RuleSet = {
    implicit val eqA: Eq[A] = laws.E
    new DefaultRuleSet(
      "referentialEq",
      None,
      "reflexivity eq" -> forAll(laws.reflexivityEq _),
      "symmetry eq" -> forAll(laws.symmetryEq _),
      "transitivity eq" -> forAll(laws.transitivityEq _)
    )
  }
}

object ReferentialEqTests {
  def apply[A: Eq]: ReferentialEqTests[A] = new ReferentialEqTests[A] {
    override def laws: EqLaws[A] = EqLaws[A]
  }
}
