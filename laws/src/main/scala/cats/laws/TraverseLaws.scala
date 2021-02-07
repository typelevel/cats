package cats
package laws

import cats.Id
import cats.data.{Const, Nested, State, StateT}
import cats.syntax.traverse._
import cats.syntax.foldable._

trait TraverseLaws[F[_]] extends FunctorLaws[F] with FoldableLaws[F] with UnorderedTraverseLaws[F] {
  implicit override def F: Traverse[F]

  def traverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.traverse[Id, B](f) <-> F.map(fa)(f)

  def traverseSequentialComposition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: B => N[C]
  )(implicit N: Applicative[N], M: Applicative[M]): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(fa.traverse(f))(fb => fb.traverse(g)))
    val rhs = fa.traverse[Nested[M, N, *], C](a => Nested(M.map(f(a))(g)))
    lhs <-> rhs
  }

  def traverseParallelComposition[A, B, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: A => N[B]
  )(implicit N: Applicative[N], M: Applicative[M]): IsEq[(M[F[B]], N[F[B]])] = {
    type MN[Z] = (M[Z], N[Z])
    implicit val MN: Applicative[MN] = new Applicative[MN] {
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

  def traverseTap[A, B, G[_]](fa: F[A], f: A => G[B])(implicit G: Applicative[G]): IsEq[G[F[A]]] =
    fa.traverseTap(f) <-> fa.traverse(a => Applicative[G].as(f(a), a))

  def foldMapDerived[A, B](
    fa: F[A],
    f: A => B
  )(implicit B: Monoid[B]): IsEq[B] = {
    val lhs: B = fa.traverse[Const[B, *], B](a => Const(f(a))).getConst
    val rhs: B = fa.foldMap(f)
    lhs <-> rhs
  }

  def traverseOrderConsistent[A](fa: F[A]): IsEq[Option[A]] = {
    class FirstOption[T](val o: Option[T])

    implicit val firstOptionMonoid: Monoid[FirstOption[A]] = new Monoid[FirstOption[A]] {
      def empty = new FirstOption(None)
      def combine(x: FirstOption[A], y: FirstOption[A]) = new FirstOption(x.o.orElse(y.o))
    }

    def liftId[T](a: T): Id[T] = a
    def store[T](a: T): Const[FirstOption[T], T] = Const(new FirstOption(Some(a)))

    val first = F.traverse[Const[FirstOption[A], *], A, A](fa)(store).getConst.o
    val traverseFirst = F
      .traverse[Const[FirstOption[A], *], A, A](
        F.traverse(fa)(liftId)
      )(store)
      .getConst
      .o

    first <-> traverseFirst
  }
  def mapWithIndexRef[A, B](fa: F[A], f: (A, Int) => B): IsEq[F[B]] = {
    val lhs = F.mapWithIndex(fa)(f)
    val rhs = F.traverse(fa)(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value
    lhs <-> rhs
  }

  def traverseWithIndexMRef[G[_], A, B](fa: F[A], f: (A, Int) => G[B])(implicit G: Monad[G]): IsEq[G[F[B]]] = {
    val lhs = F.traverseWithIndexM(fa)(f)
    val rhs = F.traverse(fa)(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b)))).runA(0)
    lhs <-> rhs
  }

  def zipWithIndexRef[A, B](fa: F[A], f: ((A, Int)) => B): IsEq[F[B]] = {
    val lhs = F.map(F.zipWithIndex(fa))(f)
    val rhs = F.map(F.mapWithIndex(fa)((a, i) => (a, i)))(f)
    lhs <-> rhs
  }
}

object TraverseLaws {
  def apply[F[_]](implicit ev: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { def F: Traverse[F] = ev }
}
