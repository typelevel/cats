package alleycats.laws.discipline

import cats._
import cats.laws.FlatMapLaws
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws


trait FlatMapRecTests[F[_]] extends Laws {
  def laws: FlatMapLaws[F]

  def tailRecM[A: Arbitrary](implicit
                             ArbFA: Arbitrary[F[A]],
                             ArbAFA: Arbitrary[A => F[A]],
                             EqFA: Eq[F[A]]
                            ): RuleSet = {
    new DefaultRuleSet(
      name = "flatMapTailRec",
      parent = None,
      "tailRecM consistent flatMap" -> forAll(laws.tailRecMConsistentFlatMap[A] _))
  }
}

object FlatMapRecTests {
  def apply[F[_]: FlatMap]: FlatMapRecTests[F] =
    new FlatMapRecTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
