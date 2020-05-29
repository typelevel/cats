package cats

import scala.collection.immutable.{Queue, SortedMap}
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * `FunctorFilter[F]` allows you to `map` and filter out elements simultaneously.
 */
@implicitNotFound("Could not find an instance of FunctorFilter for ${F}")
@typeclass
trait FunctorFilter[F[_]] extends Serializable {
  def functor: Functor[F]

  /**
   * A combined `map` and `filter`. Filtering is handled via `Option`
   * instead of `Boolean` such that the output type `B` can be different than
   * the input type `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def asString(i: Int): Option[String] = m.get(i)
   * scala> l.mapFilter(asString)
   * res0: List[String] = List(one, three)
   * }}}
   */
  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]

  /**
   * Similar to [[mapFilter]] but uses a partial function instead of a function
   * that returns an `Option`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> FunctorFilter[List].collect(l){
   *      |   case 1 => "one"
   *      |   case 3 => "three"
   *      | }
   * res0: List[String] = List(one, three)
   * }}}
   */
  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapFilter(fa)(f.lift)

  /**
   * "Flatten" out a structure by collapsing `Option`s.
   * Equivalent to using `mapFilter` with `identity`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Option[Int]] = List(Some(1), None, Some(3), None)
   * scala> l.flattenOption
   * res0: List[Int] = List(1, 3)
   * }}}
   */
  def flattenOption[A](fa: F[Option[A]]): F[A] =
    mapFilter(fa)(identity)

  /**
   * Apply a filter to a structure such that the output structure contains all
   * `A` elements in the input structure that satisfy the predicate `f` but none
   * that don't.
   */
  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    mapFilter(fa)(a => if (f(a)) Some(a) else None)

  /**
   * Apply a filter to a structure such that the output structure contains all
   * `A` elements in the input structure that do not satisfy the predicate `f`.
   */
  def filterNot[A](fa: F[A])(f: A => Boolean): F[A] =
    mapFilter(fa)(Some(_).filterNot(f))
}

object FunctorFilter extends ScalaVersionSpecificTraverseFilterInstances {
  implicit def catsTraverseFilterForOption: TraverseFilter[Option] =
    cats.instances.option.catsStdTraverseFilterForOption
  implicit def catsTraverseFilterForList: TraverseFilter[List] = cats.instances.list.catsStdTraverseFilterForList
  implicit def catsTraverseFilterForVector: TraverseFilter[Vector] =
    cats.instances.vector.catsStdTraverseFilterForVector
  implicit def catsFunctorFilterForMap[K: Order]: FunctorFilter[Map[K, *]] =
    cats.instances.map.catsStdFunctorFilterForMap[K]
  implicit def catsTraverseFilterForSortedMap[K: Order]: TraverseFilter[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdTraverseFilterForSortedMap[K]
  implicit def catsTraverseFilterForQueue: TraverseFilter[Queue] =
    cats.instances.queue.catsStdTraverseFilterForQueue

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[FunctorFilter]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: FunctorFilter[F]): FunctorFilter[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: FunctorFilter[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def mapFilter[B](f: A => Option[B]): F[B] = typeClassInstance.mapFilter[A, B](self)(f)
    def collect[B](f: PartialFunction[A, B]): F[B] = typeClassInstance.collect[A, B](self)(f)
    def flattenOption[B](implicit ev$1: A <:< Option[B]): F[B] =
      typeClassInstance.flattenOption[B](self.asInstanceOf[F[Option[B]]])
    def filter(f: A => Boolean): F[A] = typeClassInstance.filter[A](self)(f)
    def filterNot(f: A => Boolean): F[A] = typeClassInstance.filterNot[A](self)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToFunctorFilterOps extends Serializable {
    implicit def toFunctorFilterOps[F[_], A](target: F[A])(implicit tc: FunctorFilter[F]): Ops[F, A] {
      type TypeClassType = FunctorFilter[F]
    } =
      new Ops[F, A] {
        type TypeClassType = FunctorFilter[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToFunctorFilterOps
  object ops {
    implicit def toAllFunctorFilterOps[F[_], A](target: F[A])(implicit tc: FunctorFilter[F]): AllOps[F, A] {
      type TypeClassType = FunctorFilter[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = FunctorFilter[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
