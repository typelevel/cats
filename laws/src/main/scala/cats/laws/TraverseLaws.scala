package cats
package laws

import cats.Id
import cats.syntax.functor._
import cats.syntax.traverse._

trait TraverseLaws[F[_]] extends FunctorLaws[F] with FoldableLaws[F] {
  implicit override def F: Traverse[F]

  def traverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] = {
    fa.traverse[Id, B](f) <-> F.map(fa)(f)
  }

  def traverseComposition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: B => N[C]
  )(implicit
    N: Applicative[N],
    M: Applicative[M]
  ): IsEq[M[N[F[C]]]] = {
    implicit val MN = M.compose(N)
    type MN[Z] = M[N[Z]]
    val lhs: MN[F[C]] = M.map(fa.traverse(f))(fb => fb.traverse(g))
    val rhs: MN[F[C]] = fa.traverse[MN, C](a => M.map(f(a))(g))
    lhs <-> rhs
  }
}

object TraverseLaws {
  def apply[F[_]](implicit ev: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { def F: Traverse[F] = ev }
}
