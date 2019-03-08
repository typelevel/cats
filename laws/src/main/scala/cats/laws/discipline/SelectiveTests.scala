package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws

trait SelectiveTests[F[_]] extends Laws {
  def laws: SelectiveLaws[F]

  // TODO remove unneeded implicits
  def selective[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                            ArbFA: Arbitrary[F[A]],
                                                            ArbFB: Arbitrary[F[B]],
                                                            ArbFC: Arbitrary[F[C]],
                                                            ArbFEitherA: Arbitrary[F[Either[A, A]]],
                                                            ArbFAtoB: Arbitrary[F[A => B]],
                                                            ArbFBtoC: Arbitrary[F[B => C]],
                                                            CogenA: Cogen[A],
                                                            CogenB: Cogen[B],
                                                            CogenC: Cogen[C],
                                                            EqFA: Eq[F[A]],
                                                            EqFB: Eq[F[B]],
                                                            EqFC: Eq[F[C]],
                                                            EqFABC: Eq[F[(A, B, C)]],
                                                            iso: Isomorphisms[F]): RuleSet =
    new DefaultRuleSet(
      name = "selective",
      parent = None,
      "selective identity" -> forAll(laws.selectiveIdentity[A] _),
      "selective distributivity" -> forAll(laws.selectiveDistributivity[A, B] _)
    )
}

object SelectiveTests {

  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] { def laws: SelectiveLaws[F] = SelectiveLaws[F] }

  def monad[F[_]: Monad]: SelectiveTests[F] =
    new SelectiveTests[F] { def laws: SelectiveLaws[F] = SelectiveLaws[F](Selective.fromMonad) }

}
