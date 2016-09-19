package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

trait MonadStateTests[F[_], S] extends MonadTests[F] {
  def laws: MonadStateLaws[F, S]

  def monadState[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    ArbS: Arbitrary[S],
    ArbFS: Arbitrary[F[S]],
    ArbFUnit: Arbitrary[F[Unit]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFUnit: Eq[F[Unit]],
    EqFS: Eq[F[S]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadState"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadState get idempotent" -> laws.monadStateGetIdempotent,
        "monadState set twice"      -> forAll(laws.monadStateSetTwice _),
        "monadState set get"        -> forAll(laws.monadStateSetGet _),
        "monadState get set"        -> laws.monadStateGetSet
      )
    }
  }
}

object MonadStateTests {
  def apply[F[_], S](implicit FS: MonadState[F, S]): MonadStateTests[F, S] =
    new MonadStateTests[F, S] {
      def laws: MonadStateLaws[F, S] = MonadStateLaws[F, S]
    }
}
