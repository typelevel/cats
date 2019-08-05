package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait BitraverseTests[F[_, _]] extends BifoldableTests[F] with BifunctorTests[F] {
  def laws: BitraverseLaws[F]

  def bitraverse[G[_], A, B, C, D, E, H](implicit
                                         G: Applicative[G],
                                         A: Monoid[A],
                                         B: Monoid[B],
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
                                         CogenA: Cogen[A],
                                         CogenB: Cogen[B],
                                         CogenC: Cogen[C],
                                         CogenD: Cogen[D],
                                         CogenE: Cogen[E],
                                         EqFAB: Eq[F[A, B]],
                                         EqAB: Eq[(A, B)],
                                         EqFAD: Eq[F[A, D]],
                                         EqFAH: Eq[F[A, H]],
                                         EqFCD: Eq[F[C, D]],
                                         EqFCH: Eq[F[C, H]],
                                         EqGGFEH: Eq[G[G[F[E, H]]]],
                                         EqC: Eq[C]): RuleSet =
    new RuleSet {
      val name = "bitraverse"
      val parents = Seq(bifoldable[A, B, C], bifunctor[A, B, C, D, E, H])
      val bases = Seq.empty
      val props = Seq(
        "bitraverse identity" -> forAll(laws.bitraverseIdentity[A, B] _),
        "bitraverse composition" -> forAll(laws.bitraverseCompose[G, A, B, C, D, E, H] _)
      )
    }
}

object BitraverseTests {
  def apply[F[_, _]: Bitraverse]: BitraverseTests[F] =
    new BitraverseTests[F] { def laws: BitraverseLaws[F] = BitraverseLaws[F] }
}
