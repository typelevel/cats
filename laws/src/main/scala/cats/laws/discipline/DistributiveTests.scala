package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait DistributiveTests[F[_]] extends FunctorTests[F] {
  def laws: DistributiveLaws[F]

  def distributive[A: Arbitrary, B: Arbitrary, C: Arbitrary, X[_]: Functor, Y[_]: Distributive](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbXA: Arbitrary[X[A]],
    ArbYC: Arbitrary[Y[C]],
    ArbFYA: Arbitrary[F[Y[A]]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqA: Eq[A],
    EqFYXC: Eq[F[Y[X[C]]]],
    EqFYA: Eq[F[Y[A]]],
    EqYFB: Eq[Y[F[B]]]
  ): RuleSet = {
    new RuleSet {
      def name: String = "distributive"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "distributive distribute identity" -> forAll(laws.distributeIdentity[A, B] _),
        "distributive identity" -> forAll(laws.cosequenceIdentity[A] _),
        "distributive composition" -> forAll(laws.composition[A, B, C, X, Y] _),
        "distributive double cosequence identity" -> forAll(laws.cosequenceTwiceIsId[A, Y] _)
      )
    }
  }
}

object DistributiveTests {
  def apply[F[_]: Distributive]: DistributiveTests[F] =
    new DistributiveTests[F] { def laws: DistributiveLaws[F] = DistributiveLaws[F] }
}
