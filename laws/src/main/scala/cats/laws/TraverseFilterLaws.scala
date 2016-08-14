package cats
package laws

import cats.data.Nested
import cats.implicits._

trait TraverseFilterLaws[F[_]] extends TraverseLaws[F] with FunctorFilterLaws[F] {
  implicit override def F: TraverseFilter[F]

  def traverseFilterIdentity[G[_]:Applicative, A](fa: F[A]): IsEq[G[F[A]]] = {
    fa.traverseFilter(_.some.pure[G]) <-> fa.pure[G]
  }

  def traverseFilterComposition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[Option[B]],
    g: B => N[Option[C]]
  )(implicit
      M: Applicative[M],
      N: Applicative[N]
  ): IsEq[Nested[M, N, F[C]]] = {

    val lhs: Nested[M, N, F[C]] = Nested(fa.traverseFilter(f).map(_.traverseFilter(g)))
    val rhs: Nested[M, N, F[C]] = fa.traverseFilter[Nested[M, N, ?], C](a =>
      Nested(f(a).map(_.traverseFilter(g))))
    lhs <-> rhs
  }
}

object TraverseFilterLaws {
  def apply[F[_]](implicit ev: TraverseFilter[F]): TraverseFilterLaws[F] =
    new TraverseFilterLaws[F] { def F: TraverseFilter[F] = ev }
}
