package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait MonadReaderTests[F[_], R] extends MonadTests[F] {
  def laws: MonadReaderLaws[F, R]

  def monadReader[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbR: Arbitrary[R],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenR: Cogen[R],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFR: Eq[F[R]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadReader"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadReader ask idempotent" -> laws.monadReaderAskIdempotent,
        "monadReader local ask" -> forAll(laws.monadReaderLocalAsk _),
        "monadReader local pure" -> forAll(laws.monadReaderLocalPure[A] _),
        "monadReader local flatMap" -> forAll(laws.monadReaderLocalFlatMap[A, B] _),
        "monadReader reader ask" -> forAll(laws.monadReaderReaderAsk[A] _)
      )
    }
  }
}

object MonadReaderTests {
  def apply[F[_], R](implicit FR: MonadReader[F, R]): MonadReaderTests[F, R] =
    new MonadReaderTests[F, R] {
      def laws: MonadReaderLaws[F, R] = MonadReaderLaws[F, R]
    }
}
