package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait NonEmptyParallelTests[M[_]] extends Laws {
  val laws: NonEmptyParallelLaws[M]
  type F[A] = laws.F[A]

  def nonEmptyParallel[A, B](implicit
    ArbA: Arbitrary[A],
    ArbM: Arbitrary[M[A]],
    ArbMb: Arbitrary[M[B]],
    Arbf: Arbitrary[A => B],
    EqMa: Eq[M[A]],
    EqMb: Eq[M[B]],
    ArbF: Arbitrary[F[A]],
    EqFa: Eq[F[A]]
  ): RuleSet =
    new DefaultRuleSet(
      "parallel",
      None,
      "parallel round trip" -> forAll((ma: M[A]) => laws.parallelRoundTrip(ma)),
      "sequential round trip" -> forAll((fa: F[A]) => laws.sequentialRoundTrip(fa)),
      "isomorphic functor" -> forAll((fa: F[A], f: A => B) => laws.isomorphicFunctor(fa, f))
    )
}

object NonEmptyParallelTests {
  type Aux[M[_], F0[_]] = NonEmptyParallelTests[M] { type F[A] = F0[A]; val laws: NonEmptyParallelLaws.Aux[M, F0] }

  def apply[M[_]](implicit ev: NonEmptyParallel[M]): NonEmptyParallelTests.Aux[M, ev.F] =
    apply[M, ev.F](ev, implicitly)

  def apply[M[_], F[_]](implicit ev: NonEmptyParallel.Aux[M, F], D: DummyImplicit): NonEmptyParallelTests.Aux[M, F] =
    new NonEmptyParallelTests[M] { val laws = NonEmptyParallelLaws[M] }
}
