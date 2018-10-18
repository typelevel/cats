package cats
package laws
package discipline

import cats.InvariantSemigroupal
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}

trait InvariantSemigroupalTests[F[_]] extends InvariantTests[F] with SemigroupalTests[F] {
  def laws: InvariantSemigroupalLaws[F]

  def invariantSemigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                                     arbFA: Arbitrary[F[A]],
                                                                     arbFB: Arbitrary[F[B]],
                                                                     arbFC: Arbitrary[F[C]],
                                                                     CogenA: Cogen[A],
                                                                     CogenB: Cogen[B],
                                                                     CogenC: Cogen[C],
                                                                     EqFA: Eq[F[A]],
                                                                     EqFB: Eq[F[B]],
                                                                     EqFC: Eq[F[C]],
                                                                     EqFABC: Eq[F[(A, B, C)]],
                                                                     iso: Isomorphisms[F]): RuleSet = new RuleSet {
    val name = "invariantSemigroupal"
    val parents = Seq(invariant[A, B, C], semigroupal[A, B, C])
    val bases = Nil
    val props = Nil
  }
}

object InvariantSemigroupalTests {
  def apply[F[_]: InvariantSemigroupal]: InvariantSemigroupalTests[F] =
    new InvariantSemigroupalTests[F] { def laws: InvariantSemigroupalLaws[F] = InvariantSemigroupalLaws[F] }
}
