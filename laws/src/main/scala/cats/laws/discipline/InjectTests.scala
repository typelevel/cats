package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait InjectTests[A, B] extends Laws {
  def laws: InjectLaws[A, B]

  def inject(implicit
    ArbA: Arbitrary[A],
    EqOptionA: Eq[Option[A]],
    ArbB: Arbitrary[B],
    EqOptionB: Eq[Option[B]]
  ): RuleSet =
    new DefaultRuleSet(
      "inject",
      None,
      "inject round trip inj" -> forAll((a: A) => laws.injectRoundTripInj(a)),
      "inject round trip prj" -> forAll((b: B) => laws.injectRoundTripPrj(b))
    )

}

object InjectTests {
  def apply[A, B](implicit ev: Inject[A, B]): InjectTests[A, B] =
    new InjectTests[A, B] { val laws: InjectLaws[A, B] = InjectLaws[A, B] }
}
