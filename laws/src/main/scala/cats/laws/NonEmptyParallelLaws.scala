package cats
package laws

/**
 * Laws that must be obeyed by any `cats.NonEmptyParallel`.
 */
trait NonEmptyParallelLaws[M[_]] {
  val P: NonEmptyParallel[M]
  type F[A] = P.F[A]

  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    P.sequential(P.parallel(ma)) <-> ma

  def sequentialRoundTrip[A](fa: F[A]): IsEq[F[A]] =
    P.parallel(P.sequential(fa)) <-> fa

  def isomorphicFunctor[A, B](fa: F[A], f: A => B): IsEq[M[B]] =
    P.flatMap.map(P.sequential(fa))(f) <-> P.sequential(P.apply.map(fa)(f))
}

object NonEmptyParallelLaws {
  type Aux[M[_], F0[_]] = NonEmptyParallelLaws[M] { type F[A] = F0[A]; val P: NonEmptyParallel.Aux[M, F0] }

  def apply[M[_]](implicit ev: NonEmptyParallel[M]): NonEmptyParallelLaws.Aux[M, ev.F] =
    apply[M, ev.F](ev, implicitly)

  def apply[M[_], F[_]](implicit ev: NonEmptyParallel.Aux[M, F], D: DummyImplicit): NonEmptyParallelLaws.Aux[M, F] =
    new NonEmptyParallelLaws[M] { val P: ev.type = ev }
}
