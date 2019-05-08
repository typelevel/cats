package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait NonEmptyParallelTests[M[_], F[_]] extends Laws {
  def laws: NonEmptyParallelLaws[M, F]

  def nonEmptyParallel[A, B](implicit ArbA: Arbitrary[A],
                             ArbM: Arbitrary[M[A]],
                             ArbMb: Arbitrary[M[B]],
                             Arbf: Arbitrary[A => B],
                             EqMa: Eq[M[A]],
                             EqMb: Eq[M[B]],
                             ArbF: Arbitrary[F[A]],
                             EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "parallel",
      None,
      "parallel round trip" -> forAll((ma: M[A]) => laws.parallelRoundTrip(ma)),
      "sequential round trip" -> forAll((fa: F[A]) => laws.sequentialRoundTrip(fa)),
      "isomorphic functor" -> forAll((fa: F[A], f: A => B) => laws.isomorphicFunctor(fa, f))
    )
}

object NonEmptyParallelTests {
  def apply[M[_], F[_]](implicit ev: NonEmptyParallel[M, F]): NonEmptyParallelTests[M, F] =
    new NonEmptyParallelTests[M, F] { val laws: NonEmptyParallelLaws[M, F] = NonEmptyParallelLaws[M, F] }
}
