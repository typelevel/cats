package alleycats.tests

import alleycats.ReferentialEq
import cats.kernel.Eq
import cats.kernel.laws.discipline._
import cats.kernel.laws.EqLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

class ReferentialEqSuite extends AlleycatsSuite {

  class ReferentialEqTests[A](eq: Eq[A]) extends Laws {
    def laws = EqLaws(eq)

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

  implicit val arbObject: Arbitrary[Object] =
    // with some probability we select from a small set of objects
    // otherwise make a totally new one
    // courtesy of @johnynek
    Arbitrary(
      Gen.oneOf(
        Gen.oneOf(List.fill(5)(new Object)),
        Arbitrary.arbUnit.arbitrary.map(_ => new Object)
      )
    )

  implicit val eqObject: Eq[Object] = ReferentialEq[Object]

  checkAll("ReferentialEq[Object]", new ReferentialEqTests(ReferentialEq[Object]).eqv)

}
