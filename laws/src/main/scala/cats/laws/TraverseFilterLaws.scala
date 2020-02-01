package cats
package laws

import cats.data.Nested
import cats.syntax.all._
import cats.instances.option._

trait TraverseFilterLaws[F[_]] extends FunctorFilterLaws[F] {
  implicit override def F: TraverseFilter[F]

  def traverseFilterIdentity[G[_]: Applicative, A](fa: F[A]): IsEq[G[F[A]]] =
    fa.traverseFilter(_.some.pure[G]) <-> fa.pure[G]

  def traverseFilterConsistentWithTraverse[G[_]: Applicative, A](fa: F[A], f: A => G[A]): IsEq[G[F[A]]] =
    fa.traverseFilter(a => f(a).map(_.some)) <-> F.traverse.traverse(fa)(f)

  def traverseFilterComposition[A, B, C, M[_], N[_]](fa: F[A], f: A => M[Option[B]], g: B => N[Option[C]])(
    implicit
    M: Applicative[M],
    N: Applicative[N]
  ): IsEq[Nested[M, N, F[C]]] = {
    val lhs = Nested[M, N, F[C]](fa.traverseFilter(f).map(_.traverseFilter(g)))
    val rhs: Nested[M, N, F[C]] =
      fa.traverseFilter[Nested[M, N, *], C](a => Nested[M, N, Option[C]](f(a).map(_.traverseFilter(g))))
    lhs <-> rhs
  }

  def filterAConsistentWithTraverseFilter[G[_]: Applicative, A](fa: F[A], f: A => G[Boolean]): IsEq[G[F[A]]] =
    fa.filterA(f) <-> fa.traverseFilter(a => f(a).map(if (_) Some(a) else None))
}

object TraverseFilterLaws {
  def apply[F[_]](implicit ev: TraverseFilter[F]): TraverseFilterLaws[F] =
    new TraverseFilterLaws[F] { def F: TraverseFilter[F] = ev }
}
