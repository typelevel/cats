package cats
package laws


/**
  * Laws that must be obeyed by any `cats.NonEmptyParallel`.
  */
trait NonEmptyParallelLaws[M[_], F[_]] {
  def P: NonEmptyParallel[M, F]

  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    P.sequential(P.parallel(ma)) <-> ma

  def sequentialRoundTrip[A](fa: F[A]): IsEq[F[A]] =
    P.parallel(P.sequential(fa)) <-> fa

  def isomorphicFunctor[A, B](fa: F[A], f: A => B): IsEq[M[B]] =
    P.flatMap.map(P.sequential(fa))(f) <-> P.sequential(P.apply.map(fa)(f))
}

object NonEmptyParallelLaws {
  def apply[M[_], F[_]](implicit ev: NonEmptyParallel[M, F]): NonEmptyParallelLaws[M, F] =
    new NonEmptyParallelLaws[M, F] { def P: NonEmptyParallel[M, F] = ev }
}
