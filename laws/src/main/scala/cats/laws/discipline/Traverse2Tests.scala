package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait Traverse2Tests[F[_, _]] extends Foldable2Tests[F] with Functor2Tests[F] {
  def laws: Traverse2Laws[F]

  def traverse2[G[_], A, B, C, D, E, H](implicit
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
      val name = "traverse2"
      val parents = Seq(foldable2[A, B, C], functor2[A, B, C, D, E, H])
      val bases = Seq.empty
      val props = Seq(
        "traverse2 identity" -> forAll(laws.traverse2Identity[A, B] _),
        "traverse2 composition" -> forAll(laws.traverse2Compose[G, A, B, C, D, E, H] _)
      )
    }
}

object Traverse2Tests {
  def apply[F[_, _]: Traverse2]: Traverse2Tests[F] =
    new Traverse2Tests[F] { def laws: Traverse2Laws[F] = Traverse2Laws[F] }
}
