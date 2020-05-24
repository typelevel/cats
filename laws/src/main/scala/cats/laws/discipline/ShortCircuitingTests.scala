package cats.laws.discipline

import cats.laws.ShortCircuitingLaws
import cats.{Eq, Traverse, TraverseFilter}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ShortCircuitingTests[F[_]] extends Laws {
  def laws: ShortCircuitingLaws[F]

  def traverse[A: Arbitrary](implicit F: Traverse[F], ArbFA: Arbitrary[F[A]], lEq: Eq[Long]): RuleSet =
    new DefaultRuleSet(
      name = "traverseShortCircuiting",
      parent = None,
      "traverse short-circuits if Applicative[G].map2Eval shorts" -> forAll(laws.traverseShortCircuits[A] _),
      "traverse won't short-circuit if Applicative[G].map2Eval won't" -> forAll(laws.traverseWontShortCircuit[A] _)
    )

  def traverseFilter[A: Arbitrary](
    implicit TF: TraverseFilter[F],
    ArbFA: Arbitrary[F[A]],
    lEq: Eq[Long]
  ): RuleSet = {
    implicit val T: Traverse[F] = TF.traverse
    new DefaultRuleSet(
      name = "traverseFilterShortCircuiting",
      parent = Some(traverse[A]),
      "traverseFilter short-circuits if Applicative[G].map2Eval shorts" ->
        forAll(laws.traverseFilterShortCircuits[A] _),
      "traverseFilter short-circuits if Applicative[G].map2Eval won't" ->
        forAll(laws.traverseFilterWontShortCircuit[A] _),
      "filterA short-circuits if Applicative[G].map2Eval shorts" -> forAll(laws.filterAShortCircuits[A] _),
      "filterA short-circuits if Applicative[G].map2Eval won't" -> forAll(laws.filterAWontShortCircuit[A] _)
    )
  }
}

object ShortCircuitingTests {
  def apply[F[_]]: ShortCircuitingTests[F] = new ShortCircuitingTests[F] {
    override def laws: ShortCircuitingLaws[F] = ShortCircuitingLaws[F]
  }
}
