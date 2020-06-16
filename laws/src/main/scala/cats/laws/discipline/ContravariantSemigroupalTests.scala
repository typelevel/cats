package cats
package laws
package discipline

import cats.ContravariantSemigroupal
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait ContravariantSemigroupalTests[F[_]] extends ContravariantTests[F] with SemigroupalTests[F] {
  def laws: ContravariantSemigroupalLaws[F]

  def contravariantSemigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
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
    iso: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name = "contravariantSemigroupal"
      val parents = Seq(contravariant[A, B, C], semigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "contravariantSemigroupal contramap2 delta associates" ->
          forAll(laws.contravariantSemigroupalContramap2DiagonalAssociates[A] _)
      )
    }
}

object ContravariantSemigroupalTests {
  def apply[F[_]: ContravariantSemigroupal]: ContravariantSemigroupalTests[F] =
    new ContravariantSemigroupalTests[F] { def laws: ContravariantSemigroupalLaws[F] = ContravariantSemigroupalLaws[F] }
}
