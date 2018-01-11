package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait ApplicativeTests[F[_]] extends ApplyTests[F] {
  def laws: ApplicativeLaws[F]

  def applicative[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
    iso: Isomorphisms[F]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "applicative",
      parent = Some(apply[A, B, C]),
      "applicative identity" -> forAll(laws.applicativeIdentity[A] _),
      "applicative homomorphism" -> forAll(laws.applicativeHomomorphism[A, B] _),
      "applicative interchange" -> forAll(laws.applicativeInterchange[A, B] _),
      "applicative map" -> forAll(laws.applicativeMap[A, B] _),
      "applicative unit" -> forAll(laws.applicativeUnit[A] _),
      "ap consistent with product + map" -> forAll(laws.apProductConsistent[A, B] _),
      "monoidal left identity" -> forAll((fa: F[A]) => iso.leftIdentity(laws.monoidalLeftIdentity(fa))),
      "monoidal right identity" -> forAll((fa: F[A]) => iso.rightIdentity(laws.monoidalRightIdentity(fa))))
  }
}

object ApplicativeTests {
  def apply[F[_]: Applicative]: ApplicativeTests[F] =
    new ApplicativeTests[F] { def laws: ApplicativeLaws[F] = ApplicativeLaws[F] }
}
