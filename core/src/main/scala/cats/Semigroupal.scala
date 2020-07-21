package cats

import cats.kernel.CommutativeSemigroup
import scala.collection.immutable.{Queue, SortedMap, SortedSet}
import scala.util.Try
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * [[Semigroupal]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Semigroupal]] and [[Functor]] to illustrate this.
 */
@implicitNotFound("Could not find an instance of Semigroupal for ${F}")
@typeclass trait Semigroupal[F[_]] extends Serializable {

  /**
   * Combine an `F[A]` and an `F[B]` into an `F[(A, B)]` that maintains the effects of both `fa` and `fb`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val noneInt: Option[Int] = None
   * scala> val some3: Option[Int] = Some(3)
   * scala> val noneString: Option[String] = None
   * scala> val someFoo: Option[String] = Some("foo")
   *
   * scala> Semigroupal[Option].product(noneInt, noneString)
   * res0: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(noneInt, someFoo)
   * res1: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, noneString)
   * res2: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, someFoo)
   * res3: Option[(Int, String)] = Some((3,foo))
   * }}}
   */
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Semigroupal extends ScalaVersionSpecificSemigroupalInstances with SemigroupalArityFunctions {
  implicit def catsSemigroupalForOption: Semigroupal[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsSemigroupalForTry: Semigroupal[Try] = cats.instances.try_.catsStdInstancesForTry
  implicit def catsSemigroupalForList: Semigroupal[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsSemigroupalForVector: Semigroupal[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsSemigroupalForQueue: Semigroupal[Queue] = cats.instances.queue.catsStdInstancesForQueue
  implicit def catsSemigroupalForMap[K]: Semigroupal[Map[K, *]] = cats.instances.map.catsStdInstancesForMap[K]
  implicit def catsSemigroupalForEither[A]: Semigroupal[Either[A, *]] =
    cats.instances.either.catsStdInstancesForEither[A]
  implicit def catsSemigroupalForSortedSet: Semigroupal[SortedSet] =
    cats.instances.sortedSet.catsStdSemigroupalForSortedSet
  implicit def catsSemigroupalForSortedMap[K]: Semigroupal[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]
  implicit def catsSemigroupalForFunction1[A]: Semigroupal[A => *] =
    cats.instances.function.catsStdMonadForFunction1[A]
  implicit def catsSemigroupalForFunction1Contravariant[R: Monoid]: Semigroupal[* => R] =
    cats.instances.function.catsStdContravariantMonoidalForFunction1[R]
  implicit def catsSemigroupalForFunction0: Semigroupal[Function0] =
    cats.instances.function.catsStdBimonadForFunction0

  implicit val catsSemigroupalForOrder: Semigroupal[Order] = cats.instances.order.catsContravariantMonoidalForOrder
  implicit val catsSemigroupalForPartialOrder: Semigroupal[PartialOrder] =
    cats.instances.partialOrder.catsContravariantMonoidalForPartialOrder
  implicit val catsSemigroupalForOrdering: Semigroupal[Ordering] =
    cats.instances.ordering.catsContravariantMonoidalForOrdering
  implicit val catsSemigroupalForPartialOrdering: Semigroupal[PartialOrdering] =
    cats.instances.partialOrdering.catsContravariantMonoidalForPartialOrdering
  implicit val catsSemigroupalForEq: Semigroupal[Eq] = cats.instances.eq.catsDecidableForEq
  implicit val catsSemigroupalForEquiv: Semigroupal[Equiv] =
    cats.instances.equiv.catsDecidableForEquiv
  implicit val catsSemigroupalForMonoid: Semigroupal[Monoid] =
    cats.instances.invariant.catsSemigroupalForMonoid
  implicit val catsSemigroupalForSemigroup: Semigroupal[Semigroup] =
    cats.instances.invariant.catsInvariantMonoidalSemigroup
  implicit val catsSemigroupalForCommutativeSemigroup: Semigroupal[CommutativeSemigroup] =
    cats.instances.invariant.catsInvariantMonoidalCommutativeSemigroup

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Semigroupal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Semigroupal[F]): Semigroupal[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllSemigroupalOps[F[_], A](target: F[A])(implicit tc: Semigroupal[F]): AllOps[F, A] {
      type TypeClassType = Semigroupal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Semigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Semigroupal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def product[B](fb: F[B]): F[(A, B)] = typeClassInstance.product[A, B](self, fb)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToSemigroupalOps extends Serializable {
    implicit def toSemigroupalOps[F[_], A](target: F[A])(implicit tc: Semigroupal[F]): Ops[F, A] {
      type TypeClassType = Semigroupal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Semigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToSemigroupalOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
