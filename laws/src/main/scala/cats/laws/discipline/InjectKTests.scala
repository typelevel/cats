package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait InjectKTests[F[_], G[_]] extends Laws {
  def laws: InjectKLaws[F, G]

  def injectK[A](implicit
                 ArbFA: Arbitrary[F[A]],
                 EqOptionFA: Eq[Option[F[A]]],
                 ArbGA: Arbitrary[G[A]],
                 EqOptionGA: Eq[Option[G[A]]]): RuleSet =
    new DefaultRuleSet(
      "injectK",
      None,
      "injectK round trip inj" -> forAll((fa: F[A]) => laws.injectKRoundTripInj(fa)),
      "injectK round trip prj" -> forAll((ga: G[A]) => laws.injectKRoundTripPrj(ga))
    )

}

object InjectKTests {
  def apply[F[_], G[_]](implicit ev: InjectK[F, G]): InjectKTests[F, G] =
    new InjectKTests[F, G] { val laws: InjectKLaws[F, G] = InjectKLaws[F, G] }
}
