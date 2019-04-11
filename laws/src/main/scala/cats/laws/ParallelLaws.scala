package cats
package laws

/**
 * Laws that must be obeyed by any `cats.Parallel`.
 */
trait ParallelLaws[M[_], F[_]] extends NonEmptyParallelLaws[M, F] {
  def P: Parallel[M, F]

  def isomorphicPure[A](a: A): IsEq[F[A]] =
    P.applicative.pure(a) <-> P.parallel(P.monad.pure(a))
}

object ParallelLaws {
  def apply[M[_], F[_]](implicit ev: Parallel[M, F]): ParallelLaws[M, F] =
    new ParallelLaws[M, F] { def P: Parallel[M, F] = ev }
}
