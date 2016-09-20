package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
// import Prop._

trait MonadCombineTests[F[_]] extends MonadFilterTests[F] with AlternativeTests[F] {
  def laws: MonadCombineLaws[F]

  def monadCombine[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadCombine"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monadFilter[A, B, C], alternative[A, B, C])
      def props: Seq[(String, Prop)] = Nil
      // def props: Seq[(String, Prop)] = Seq(
      //   "monadCombine left distributivity" -> forAll(laws.monadCombineLeftDistributivity[A, B] _)
      // )
    }
  }
}

object MonadCombineTests {
  def apply[F[_]: MonadCombine]: MonadCombineTests[F] =
    new MonadCombineTests[F] {
      def laws: MonadCombineLaws[F] = MonadCombineLaws[F]
    }
}
