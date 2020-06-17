package cats

import simulacrum.{noop, typeclass}
import scala.annotation.implicitNotFound

/**
 * `TraverseFilter`, also known as `Witherable`, represents list-like structures
 * that can essentially have a `traverse` and a `filter` applied as a single
 * combined operation (`traverseFilter`).
 *
 * Based on Haskell's [[https://hackage.haskell.org/package/witherable-0.1.3.3/docs/Data-Witherable.html Data.Witherable]]
 */

@implicitNotFound("Could not find an instance of TraverseFilter for ${F}")
@typeclass
trait TraverseFilter[F[_]] extends FunctorFilter[F] {
  def traverse: Traverse[F]

  final override def functor: Functor[F] = traverse

  /**
   * A combined [[traverse]] and [[filter]]. Filtering is handled via `Option`
   * instead of `Boolean` such that the output type `B` can be different than
   * the input type `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def asString(i: Int): Eval[Option[String]] = Now(m.get(i))
   * scala> val result: Eval[List[String]] = l.traverseFilter(asString)
   * scala> result.value
   * res0: List[String] = List(one, three)
   * }}}
   */
  def traverseFilter[G[_], A, B](fa: F[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[F[B]]

  /**
   * {{{
   * scala> import cats.implicits._
   * scala> val a: List[Either[String, Option[Int]]] = List(Right(Some(1)), Right(Some(5)), Right(Some(3)))
   * scala> val b: Either[String, List[Int]] = TraverseFilter[List].sequenceFilter(a)
   * b: Either[String, List[Int]] = Right(List(1, 5, 3))
   * }}}
   */
  @noop
  def sequenceFilter[G[_], A](fgoa: F[G[Option[A]]])(implicit G: Applicative[G]): G[F[A]] =
    traverseFilter(fgoa)(identity)

  /**
   * Filter values inside a `G` context.
   *
   * This is a generalized version of Haskell's [[http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html#v:filterM filterM]].
   * [[http://stackoverflow.com/questions/28872396/haskells-filterm-with-filterm-x-true-false-1-2-3 This StackOverflow question]] about `filterM` may be helpful in understanding how it behaves.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def odd(i: Int): Eval[Boolean] = Now(i % 2 == 1)
   * scala> val res: Eval[List[Int]] = l.filterA(odd)
   * scala> res.value
   * res0: List[Int] = List(1, 3)
   *
   * scala> List(1, 2, 3).filterA(_ => List(true, false))
   * res1: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(1, 3), List(1), List(2, 3), List(2), List(3), List())
   * }}}
   */
  def filterA[G[_], A](fa: F[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[F[A]] =
    traverseFilter(fa)(a => G.map(f(a))(if (_) Some(a) else None))

  /**
   * Like [[traverseFilter]], but uses `Either` instead of `Option` and allows for an action to be run on each filtered value.
   */
  def traverseEither[G[_], A, B, E](
    fa: F[A]
  )(f: A => G[Either[E, B]])(g: (A, E) => G[Unit])(implicit G: Monad[G]): G[F[B]] =
    traverseFilter(fa)(a =>
      G.flatMap(f(a)) {
        case Left(e)  => G.as(g(a, e), Option.empty[B])
        case Right(b) => G.pure(Some(b))
      }
    )

  override def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] =
    traverseFilter[Id, A, B](fa)(f)
}

object TraverseFilter {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[TraverseFilter]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: TraverseFilter[F]): TraverseFilter[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: TraverseFilter[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def traverseFilter[G[_], B](f: A => G[Option[B]])(implicit G: Applicative[G]): G[F[B]] =
      typeClassInstance.traverseFilter[G, A, B](self)(f)(G)
    def filterA[G[_]](f: A => G[Boolean])(implicit G: Applicative[G]): G[F[A]] =
      typeClassInstance.filterA[G, A](self)(f)(G)
    def traverseEither[G[_], B, C](f: A => G[Either[C, B]])(g: (A, C) => G[Unit])(implicit G: Monad[G]): G[F[B]] =
      typeClassInstance.traverseEither[G, A, B, C](self)(f)(g)(G)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with FunctorFilter.AllOps[F, A] {
    type TypeClassType <: TraverseFilter[F]
  }
  trait ToTraverseFilterOps extends Serializable {
    implicit def toTraverseFilterOps[F[_], A](target: F[A])(implicit tc: TraverseFilter[F]): Ops[F, A] {
      type TypeClassType = TraverseFilter[F]
    } =
      new Ops[F, A] {
        type TypeClassType = TraverseFilter[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToTraverseFilterOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllTraverseFilterOps[F[_], A](target: F[A])(implicit tc: TraverseFilter[F]): AllOps[F, A] {
      type TypeClassType = TraverseFilter[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = TraverseFilter[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
