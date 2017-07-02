package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

import cats.data.Ior

trait AlignTests[F[_]] extends FunctorTests[F] {
  def laws: AlignLaws[F]

  def align[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](
    implicit ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[A => C],
    ArbFBtoC: Arbitrary[B => D],
    ArbIorABtoC: Arbitrary[A Ior B => C],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFIorAA: Eq[F[A Ior A]],
    EqFIorAB: Eq[F[A Ior B]],
    EqFIorCD: Eq[F[C Ior D]]
  ): RuleSet = new DefaultRuleSet(
    name = "align",
    parent = Some(functor[A, B, C]),
    "nil left identity" -> forAll(laws.nilLeftIdentity[A, B] _),
    "nil right identity" -> forAll(laws.nilRightIdentity[A, B] _),
    "align self both" -> forAll(laws.alignSelfBoth[A] _),
    "align homomorphism" -> forAll { (fa: F[A], fb: F[B], f: A => C, g: B => D) =>
      laws.alignHomomorphism[A, B, C, D](fa, fb, f, g)
    },
    "alignWith consistent" -> forAll { (fa: F[A], fb: F[B], f: A Ior B => C) =>
      laws.alignWithConsistent[A, B, C](fa, fb, f)
    })
}

object AlignTests {
  def apply[F[_]: Align]: AlignTests[F] =
    new AlignTests[F] { def laws: AlignLaws[F] = AlignLaws[F] }
}
