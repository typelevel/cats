package cats
package laws

import cats.Id
import cats.arrow.Compose
import cats.syntax.functor._
import cats.syntax.traverse._

trait TraverseLaws[F[_]] extends FunctorLaws[F] with FoldableLaws[F] {
  implicit override def F: Traverse[F]

  def traverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] = {
    fa.traverse[Id, B](f) <-> F.map(fa)(f)
  }

  def traverseSequentialComposition[A, B, C, M[_], N[_]](
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

  def traverseParallelComposition[A, B, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: A => N[B]
  )(implicit
    N: Applicative[N],
    M: Applicative[M]
  ): IsEq[(M[F[B]], N[F[B]])] = {
    type MN[Z] = (M[Z], N[Z])
    implicit val MN = new Applicative[MN] {
      override def pure[X](x: X): MN[X] = (M.pure(x), N.pure(x))
      override def ap[X, Y](fa: MN[X])(f: MN[X => Y]): MN[Y] = {
        val (fam, fan) = fa
        val (fm, fn) = f
        (M.ap(fam)(fm), N.ap(fan)(fn))
      }
    }
    val lhs: MN[F[B]] = fa.traverse[MN, B](a => (f(a), g(a)))
    val rhs: MN[F[B]] = (fa.traverse(f), fa.traverse(g))
    lhs <-> rhs
  }
}

object TraverseLaws {
  def apply[F[_]](implicit ev: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { def F: Traverse[F] = ev }
}
