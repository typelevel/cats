package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ParallelTests[M[_], F[_], A] extends Laws {
  def laws: ParallelLaws[M, F]

  def parallel(implicit ArbM: Arbitrary[M[A]], EqMa: Eq[M[A]], ArbF: Arbitrary[F[A]], EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "parallel",
      None,
      "parallel round trip" -> forAll((ma: M[A]) => laws.parallelRoundTrip(ma)),
      "sequential round trip" -> forAll((fa: F[A]) => laws.sequentialRoundTrip(fa))
    )
}

object ParallelTests {
  def apply[M[_]: Monad, F[_], A](implicit ev: Parallel[M, F]): ParallelTests[M, F, A] =
    new ParallelTests[M, F, A] { val laws: ParallelLaws[M, F] = ParallelLaws[M, F] }
}
