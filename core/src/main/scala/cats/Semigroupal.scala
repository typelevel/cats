/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

import cats.kernel.CommutativeSemigroup
import scala.collection.immutable.{Queue, Seq, SortedMap, SortedSet}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

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
trait Semigroupal[F[_]] extends Serializable {

  /**
   * Combine an `F[A]` and an `F[B]` into an `F[(A, B)]` that maintains the effects of both `fa` and `fb`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
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
  implicit def catsSemigroupalForId: Semigroupal[Id] = catsInstancesForId
  implicit def catsSemigroupalForOption: Semigroupal[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsSemigroupalForTry: Semigroupal[Try] = cats.instances.try_.catsStdInstancesForTry

  /**
   * @deprecated
   *   Any non-pure use of [[scala.concurrent.Future Future]] with Cats is error prone
   *   (particularly the semantics of [[cats.Traverse#traverse traverse]] with regard to execution order are unspecified).
   *   We recommend using [[https://typelevel.org/cats-effect/ Cats Effect `IO`]] as a replacement for ''every'' use case of [[scala.concurrent.Future Future]].
   *   However, at this time there are no plans to remove these instances from Cats.
   *
   * @see [[https://github.com/typelevel/cats/issues/4176 Changes in Future traverse behavior between 2.6 and 2.7]]
   */
  implicit def catsSemigroupalForFuture(implicit ec: ExecutionContext): Semigroupal[Future] =
    cats.instances.future.catsStdInstancesForFuture(ec)

  implicit def catsSemigroupalForList: Semigroupal[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsSemigroupalForSeq: Semigroupal[Seq] = cats.instances.seq.catsStdInstancesForSeq
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
  implicit val catsSemigroupalForEq: Semigroupal[Eq] = cats.instances.eq.catsContravariantMonoidalForEq
  implicit val catsSemigroupalForEquiv: Semigroupal[Equiv] =
    cats.instances.equiv.catsContravariantMonoidalForEquiv
  implicit val catsSemigroupalForMonoid: Semigroupal[Monoid] =
    cats.instances.invariant.catsSemigroupalForMonoid
  implicit val catsSemigroupalForSemigroup: Semigroupal[Semigroup] =
    cats.instances.invariant.catsInvariantMonoidalSemigroup
  implicit val catsSemigroupalForCommutativeSemigroup: Semigroupal[CommutativeSemigroup] =
    cats.instances.invariant.catsInvariantMonoidalCommutativeSemigroup

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

}
