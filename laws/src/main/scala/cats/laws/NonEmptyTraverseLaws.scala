package cats.laws

import cats.{Apply, Id, NonEmptyTraverse, Semigroup}
import cats.data.{Const, Nested}
import cats.syntax.nonEmptyTraverse._
import cats.syntax.reducible._

trait NonEmptyTraverseLaws[F[_]] extends TraverseLaws[F] with ReducibleLaws[F] {
  implicit override def F: NonEmptyTraverse[F]

  def nonEmptyTraverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.nonEmptyTraverse[Id, B](f) <-> F.map(fa)(f)

  def nonEmptyTraverseSequentialComposition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: B => N[C]
  )(implicit N: Apply[N], M: Apply[M]): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(fa.nonEmptyTraverse(f))(fb => fb.nonEmptyTraverse(g)))
    val rhs = fa.nonEmptyTraverse[Nested[M, N, *], C](a => Nested(M.map(f(a))(g)))
    lhs <-> rhs
  }

  def nonEmptyTraverseParallelComposition[A, B, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: A => N[B]
  )(implicit N: Apply[N], M: Apply[M]): IsEq[(M[F[B]], N[F[B]])] = {
    type MN[Z] = (M[Z], N[Z])
    implicit val MN: Apply[MN] = new Apply[MN] {
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
    val lhs: MN[F[B]] = fa.nonEmptyTraverse[MN, B](a => (f(a), g(a)))
    val rhs: MN[F[B]] = (fa.nonEmptyTraverse(f), fa.nonEmptyTraverse(g))
    lhs <-> rhs
  }

  def reduceMapDerived[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Semigroup[B]): IsEq[B] = {
    val lhs: B = fa.nonEmptyTraverse[Const[B, *], B](a => Const(f(a))).getConst
    val rhs: B = fa.reduceMap(f)
    lhs <-> rhs
  }
}

object NonEmptyTraverseLaws {
  def apply[F[_]](implicit ev: NonEmptyTraverse[F]): NonEmptyTraverseLaws[F] =
    new NonEmptyTraverseLaws[F] { def F: NonEmptyTraverse[F] = ev }
}
