package cats
package laws


/**
  * Laws that must be obeyed by any `cats.Parallel`.
  */
trait ParallelLaws[M[_], F[_]] {
  def P: Parallel[M, F]

  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    P.sequential(P.parallel(ma)) <-> ma

  def sequentialRoundTrip[A](fa: F[A]): IsEq[F[A]] =
    P.parallel(P.sequential(fa)) <-> fa

  def isomorphicPure[A](a: A): IsEq[F[A]] =
    P.applicative.pure(a) <-> P.parallel(P.monad.pure(a))
}

object ParallelLaws {
  def apply[M[_]: Monad, F[_]](implicit ev: Parallel[M, F]): ParallelLaws[M, F] =
    new ParallelLaws[M, F] { def P: Parallel[M, F] = ev;  def monadM: Monad[M] = Monad[M] }
}
