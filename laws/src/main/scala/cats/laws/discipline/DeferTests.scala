package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Prop}
import Prop._
import org.typelevel.discipline.Laws

trait DeferTests[F[_]] extends Laws {
  def laws: DeferLaws[F]

  def defer[A: Arbitrary](implicit ArbFA: Arbitrary[F[A]], EqFA: Eq[F[A]], EqBool: Eq[Boolean]): RuleSet =
    new DefaultRuleSet(
      name = "defer",
      parent = None,
      "defer Identity" -> forAll(laws.deferIdentity[A] _),
      "defer does not evaluate" -> forAll(laws.deferDoesNotEvaluate[A] _),
      "defer is stack safe" -> forAll(laws.deferIsStackSafe[A] _)
    )
}

object DeferTests {
  def apply[F[_]: Defer]: DeferTests[F] =
    new DeferTests[F] { def laws: DeferLaws[F] = DeferLaws[F] }
}
