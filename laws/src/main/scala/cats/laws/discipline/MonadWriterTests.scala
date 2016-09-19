package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait MonadWriterTests[F[_], W] extends MonadTests[F] {
  def laws: MonadWriterLaws[F, W]

  def monadWriter[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFAW: Eq[F[(W, A)]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFU: Eq[F[Unit]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    WA: Arbitrary[W],
    WM: Monoid[W],
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      def name = "monadWriter"
      def bases = Nil
      def parents = Seq(monad[A, B, C])
      def props = Seq(
        "monadWriter writer pure" -> forAll(laws.monadWriterWriterPure[A] _),
        "monadWriter tell fusion" -> forAll(laws.monadWriterTellFusion _),
        "monadWriter listen pure" -> forAll(laws.monadWriterListenPure[A] _),
        "monadWriter listen writer" -> forAll(laws.monadWriterListenWriter[A] _)
      )
    }
}

object MonadWriterTests {
  def apply[F[_], W](implicit FW: MonadWriter[F, W]): MonadWriterTests[F, W] =
    new MonadWriterTests[F, W] {
      def laws: MonadWriterLaws[F, W] = MonadWriterLaws[F, W]
    }
}
