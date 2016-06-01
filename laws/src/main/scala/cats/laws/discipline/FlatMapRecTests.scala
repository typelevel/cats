package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait FlatMapRecTests[F[_]] extends FlatMapTests[F] {
  def laws: FlatMapRecLaws[F]

  def flatMapRec[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "flatMapRec",
      parent = Some(flatMap[A, B, C]),
      "tailRecM consistent flatMap" -> forAll(laws.tailRecMConsistentFlatMap[A] _))
  }
}

object FlatMapRecTests {
  def apply[F[_]: FlatMapRec]: FlatMapRecTests[F] =
    new FlatMapRecTests[F] { def laws: FlatMapRecLaws[F] = FlatMapRecLaws[F] }
}
