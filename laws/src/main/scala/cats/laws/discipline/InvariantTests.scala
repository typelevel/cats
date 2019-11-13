package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import cats.Invariant
import org.typelevel.discipline.Laws

trait InvariantTests[F[_]] extends Laws {
  def laws: InvariantLaws[F]

  def invariant[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                          ArbFA: Arbitrary[F[A]],
                                                          CogenA: Cogen[A],
                                                          CogenB: Cogen[B],
                                                          CogenC: Cogen[C],
                                                          EqFA: Eq[F[A]],
                                                          EqFC: Eq[F[C]]): RuleSet =
    new DefaultRuleSet(
      name = "invariant",
      parent = None,
      "invariant identity" -> forAll(laws.invariantIdentity[A] _),
      "invariant composition" -> forAll(laws.invariantComposition[A, B, C] _)
    )
}

object InvariantTests {
  def apply[F[_]: Invariant]: InvariantTests[F] =
    new InvariantTests[F] { def laws: InvariantLaws[F] = InvariantLaws[F] }
}
