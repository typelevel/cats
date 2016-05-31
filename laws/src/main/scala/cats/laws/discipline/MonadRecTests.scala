package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop

trait MonadRecTests[F[_]] extends MonadTests[F] with FlatMapRecTests[F] {
  def laws: MonadRecLaws[F]

  def monadRec[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
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
    new RuleSet {
      def name: String = "monadRec"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], flatMapRec[A, B, C])
      def props: Seq[(String, Prop)] = Nil
    }
  }
}

object MonadRecTests {
  def apply[F[_]: MonadRec]: MonadRecTests[F] =
    new MonadRecTests[F] {
      def laws: MonadRecLaws[F] = MonadRecLaws[F]
    }
}
