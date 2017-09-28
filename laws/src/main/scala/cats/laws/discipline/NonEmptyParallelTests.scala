package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait NonEmptyParallelTests[M[_], F[_], A] extends Laws {
  def laws: NonEmptyParallelLaws[M, F]

  def nonEmptyParallel
  (implicit ArbA: Arbitrary[A], ArbM: Arbitrary[M[A]], EqMa: Eq[M[A]], ArbF: Arbitrary[F[A]], EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "parallel",
      None,
      "parallel round trip" -> forAll((ma: M[A]) => laws.parallelRoundTrip(ma)),
      "sequential round trip" -> forAll((fa: F[A]) => laws.sequentialRoundTrip(fa))
    )
}

object NonEmptyParallelTests {
  def apply[M[_], F[_], A](implicit ev: NonEmptyParallel[M, F]): NonEmptyParallelTests[M, F, A] =
    new NonEmptyParallelTests[M, F, A] { val laws: NonEmptyParallelLaws[M, F] = NonEmptyParallelLaws[M, F] }
}
