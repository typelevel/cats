package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ParallelTests[M[_], F[_], A] extends NonEmptyParallelTests[M, F, A] {
  def laws: ParallelLaws[M, F]

  def parallel
  (implicit ArbA: Arbitrary[A], ArbM: Arbitrary[M[A]], Arbf: Arbitrary[A => A], EqMa: Eq[M[A]], ArbF: Arbitrary[F[A]], EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "parallel",
      Some(nonEmptyParallel),
      "isomorphic pure" -> forAll((a: A) => laws.isomorphicPure(a))
    )
}

object ParallelTests {
  def apply[M[_], F[_], A](implicit ev: Parallel[M, F]): ParallelTests[M, F, A] =
    new ParallelTests[M, F, A] { val laws: ParallelLaws[M, F] = ParallelLaws[M, F] }
}
