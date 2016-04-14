package cats.laws.discipline

import cats.{Eq, Monoid, Applicative, Copair}
import cats.laws.CopairLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait CopairTests[F[_,_]] extends BitraverseTests[F] with BifoldableTests[F] with BifunctorTests[F] {
  def laws: CopairLaws[F]

  def copair[G[_], A, B, C, D, E, H](implicit
    G: Applicative[G],
    C: Monoid[C],
    ArbFAB: Arbitrary[F[A, B]],
    ArbFAD: Arbitrary[F[A, D]],
    ArbGC: Arbitrary[G[C]],
    ArbGD: Arbitrary[G[D]],
    ArbGE: Arbitrary[G[E]],
    ArbGH: Arbitrary[G[H]],
    ArbA: Arbitrary[A],
    ArbB: Arbitrary[B],
    ArbC: Arbitrary[C],
    ArbE: Arbitrary[E],
    ArbH: Arbitrary[H],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAH: Eq[F[A, H]],
    EqFCD: Eq[F[C, D]],
    EqFCH: Eq[F[C, H]],
    EqGGFEH: Eq[G[G[F[E, H]]]],
    EqC: Eq[C]
  ): RuleSet =
    new RuleSet {
      val name = "copair"
      val parents = Seq(bitraverse[G,A,B,C,D,E,H], bifoldable[A, B, C], bifunctor[A, B, C, D, E, H])
      val bases = Seq.empty
      val props = Seq(
        "copair fold identity" -> forAll(laws.copairFoldIdentity[A, B] _),
        "copair swap identity" -> forAll(laws.copairSwapIdentity[A,B] _),
        "copair left identity" -> forAll(laws.copairLeftIdentity[A,B,C] _),
        "copair right identity" -> forAll(laws.copairRightIdentity[A,B,C] _)
      )
    }
}

object CopairTests {
  def apply[F[_, _]: Copair]: CopairTests[F] =
    new CopairTests[F] { def laws: CopairLaws[F] = CopairLaws[F] }
}
