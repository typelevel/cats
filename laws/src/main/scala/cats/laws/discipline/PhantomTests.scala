package cats
package laws
package discipline

import cats.functor.Phantom
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait PhantomTests[F[_]] extends ContravariantTests[F] {
  def laws: PhantomLaws[F]

  def phantom[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "phantom",
      parent = Some(contravariant[A, B, C]),
      "phantom identity" -> forAll(laws.phantomIdentity[A, C] _)
    )
  }
}

object PhantomTests {
  def apply[F[_]: Phantom]: PhantomTests[F] =
    new PhantomTests[F] { def laws: PhantomLaws[F] = PhantomLaws[F] }
}

