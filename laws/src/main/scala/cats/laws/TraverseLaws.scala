package cats
package laws

import cats.Id
import cats.data.{Const, Nested}
import cats.syntax.traverse._
import cats.syntax.foldable._

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
  ): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(fa.traverse(f))(fb => fb.traverse(g)))
    val rhs = fa.traverse[Nested[M, N, ?], C](a => Nested(M.map(f(a))(g)))
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
      def pure[X](x: X): MN[X] = (M.pure(x), N.pure(x))
      def ap[X, Y](f: MN[X => Y])(fa: MN[X]): MN[Y] = {
        val (fam, fan) = fa
        val (fm, fn) = f
        (M.ap(fm)(fam), N.ap(fn)(fan))
      }
      override def map[X, Y](fx: MN[X])(f: X => Y): MN[Y] = {
        val (mx, nx) = fx
        (M.map(mx)(f), N.map(nx)(f))
      }
      override def product[X, Y](fx: MN[X], fy: MN[Y]): MN[(X, Y)] = {
        val (mx, nx) = fx
        val (my, ny) = fy
        (M.product(mx, my), N.product(nx, ny))
      }
    }
    val lhs: MN[F[B]] = fa.traverse[MN, B](a => (f(a), g(a)))
    val rhs: MN[F[B]] = (fa.traverse(f), fa.traverse(g))
    lhs <-> rhs
  }

  def foldMapDerived[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Monoid[B]): IsEq[B] = {
    val lhs: B = fa.traverse[Const[B, ?], B](a => Const(f(a))).getConst
    val rhs: B = fa.foldMap(f)
    lhs <-> rhs
  }
}

object TraverseLaws {
  def apply[F[_]](implicit ev: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { def F: Traverse[F] = ev }
}
