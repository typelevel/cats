package cats.laws


import cats.{Apply, Id, Semigroup, Traverse1}
import cats.data.{Const, Nested}
import cats.syntax.traverse1._
import cats.syntax.reducible._

trait Traverse1Laws[F[_]] extends TraverseLaws[F] with ReducibleLaws[F] {
  implicit override def F: Traverse1[F]

  def traverse1Identity[A, B](fa: F[A], f: A => B): IsEq[F[B]] = {
    fa.traverse1[Id, B](f) <-> F.map(fa)(f)
  }

  def traverse1SequentialComposition[A, B, C, M[_], N[_]](
                                                          fa: F[A],
                                                          f: A => M[B],
                                                          g: B => N[C]
                                                        )(implicit
                                                          N: Apply[N],
                                                          M: Apply[M]
                                                        ): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(fa.traverse1(f))(fb => fb.traverse1(g)))
    val rhs = fa.traverse1[Nested[M, N, ?], C](a => Nested(M.map(f(a))(g)))
    lhs <-> rhs
  }

  def traverse1ParallelComposition[A, B, M[_], N[_]](
                                                     fa: F[A],
                                                     f: A => M[B],
                                                     g: A => N[B]
                                                   )(implicit
                                                     N: Apply[N],
                                                     M: Apply[M]
                                                   ): IsEq[(M[F[B]], N[F[B]])] = {
    type MN[Z] = (M[Z], N[Z])
    implicit val MN = new Apply[MN] {
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
    val lhs: MN[F[B]] = fa.traverse1[MN, B](a => (f(a), g(a)))
    val rhs: MN[F[B]] = (fa.traverse1(f), fa.traverse1(g))
    lhs <-> rhs
  }

  def reduceMapDerived[A, B](
                            fa: F[A],
                            f: A => B
                          )(implicit B: Semigroup[B]): IsEq[B] = {
    val lhs: B = fa.traverse1[Const[B, ?], B](a => Const(f(a))).getConst
    val rhs: B = fa.reduceMap(f)
    lhs <-> rhs
  }
}

object Traverse1Laws {
  def apply[F[_]](implicit ev: Traverse1[F]): Traverse1Laws[F] =
    new Traverse1Laws[F] { def F: Traverse1[F] = ev }
}
