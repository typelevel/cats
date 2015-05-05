package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait CoflatMapTests[F[_]] extends Laws {
  def laws: CoflatMapLaws[F]

  def coflatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbF: ArbitraryK[F],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    implicit def ArbFA: Arbitrary[F[A]] = ArbF.synthesize[A]

    new RuleSet {
      def name: String = "coflatMap"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Nil
      def props: Seq[(String, Prop)] = Seq(
        "coflatMap associativity" -> forAll(laws.coflatMapAssociativity[A, B, C] _)
      )
    }
  }
}

object CoflatMapTests {
  def apply[F[_]: CoflatMap]: CoflatMapTests[F] =
    new CoflatMapTests[F] { def laws: CoflatMapLaws[F] = CoflatMapLaws[F] }
}
