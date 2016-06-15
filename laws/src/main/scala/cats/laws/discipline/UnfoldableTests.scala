package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait UnfoldableTests[F[_]] extends Laws {
  def laws: UnfoldableLaws[F]

  def unfoldable[A: Arbitrary](implicit EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      name = "unfoldable",
      parent = None,
//        "noneConsistentWithDefault"         -> forAll(() => laws.noneConsistentWithDefault[A]),
        "singletonConsistentWithDefault"    -> forAll(laws.singletonConsistentWithDefault[A] _),
        "replicateConsistentWithDefault"    -> forAll(laws.replicateConsistentWithDefault[A] _),
        "buildConsistentWithDefault"        -> forAll(laws.buildConsistentWithDefault[A] _),
        "fromFoldableConsistentWithDefault" -> forAll(laws.fromFoldableConsistentWithDefault[A] _)
      )
}


object UnfoldableTests {
  def apply[F[_]: Unfoldable]: UnfoldableTests[F] =
    new UnfoldableTests[F] { def laws: UnfoldableLaws[F] = UnfoldableLaws[F] }
}
