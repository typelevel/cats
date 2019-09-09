package cats
package laws

/**
 * Laws that must be obeyed by any `cats.Parallel`.
 */
trait ParallelLaws[M[_]] extends NonEmptyParallelLaws[M] {
  val P: Parallel[M]

  def isomorphicPure[A](a: A): IsEq[F[A]] =
    P.applicative.pure(a) <-> P.parallel(P.monad.pure(a))
}

object ParallelLaws {
  type Aux[M[_], F0[_]] = ParallelLaws[M] { type F[A] = F0[A]; val P: Parallel.Aux[M, F0] }

  def apply[M[_]](implicit ev: Parallel[M]): ParallelLaws.Aux[M, ev.F] =
    Aux[M, ev.F](ev)

  def Aux[M[_], F[_]](implicit ev: Parallel.Aux[M, F]): ParallelLaws.Aux[M, F] =
    new ParallelLaws[M] { val P: ev.type = ev }
}
