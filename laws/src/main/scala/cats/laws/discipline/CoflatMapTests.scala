package cats.laws
package discipline

import cats.{CoflatMap, Eq}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
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
      def name = "coflatMap"
      def bases = Nil
      def parents = Nil
      def props = Seq(
        "coflatMap associativity" -> forAll(laws.coflatMapAssociativity[A, B, C] _)
      )
    }
  }
}

object CoflatMapTests {
  def apply[F[_]: CoflatMap]: CoflatMapTests[F] =
    new CoflatMapTests[F] { def laws = CoflatMapLaws[F] }
}
