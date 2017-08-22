package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ParallelTests[M[_], F[_]] extends Laws {
  def laws: ParallelLaws[M, F]

  def parallel[A](implicit ArbM: Arbitrary[M[A]], EqMa: Eq[M[A]]): RuleSet = new DefaultRuleSet(
    "parallel",
    None,
    "parallel round trip" -> forAll((ma: M[A]) => laws.parallelRoundTrip(ma))
  )
}

object ParallelTests {
  def apply[M[_]: Monad, F[_]](implicit ev: Parallel[M, F]): ParallelTests[M, F] =
    new ParallelTests[M, F] { val laws: ParallelLaws[M, F] = ParallelLaws[M, F] }
}
