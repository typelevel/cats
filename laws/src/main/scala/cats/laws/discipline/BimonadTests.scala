package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

trait BimonadTests[F[_]] extends MonadTests[F] with ComonadTests[F] {
  def laws: BimonadLaws[F]

  def bimonad[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFFA: Arbitrary[F[F[A]]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    EqFFFA: Eq[F[F[A]]],
    EqFFA: Eq[F[F[F[A]]]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "bimonad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], comonad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "pure andThen extract = id" -> forAll(laws.pureExtractIsId[A] _),
        "extract/flatMap entwining" -> forAll(laws.extractFlatMapEntwining[A] _),
        "pure/coflatMap entwining" -> forAll(laws.pureCoflatMapEntwining[A] _)
      )
    }
  }
}

object BimonadTests {
  def apply[F[_]: Bimonad]: BimonadTests[F] =
    new BimonadTests[F] {
      def laws: BimonadLaws[F] = BimonadLaws[F]
    }
}
