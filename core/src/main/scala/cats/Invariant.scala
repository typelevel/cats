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

import cats.arrow.Arrow
import cats.kernel._

import cats.kernel.compat.scalaVersionSpecific._
import org.typelevel.scalaccompat.annotation.targetName3
import scala.collection.immutable.{Queue, Seq, SortedMap}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.TailCalls.TailRec

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
trait Invariant[F[_]] extends Serializable { self =>

  /**
   * Transform an `F[A]` into an `F[B]` by providing a transformation from `A`
   * to `B` and one from `B` to `A`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.concurrent.duration._
   *
   * scala> val durSemigroup: Semigroup[FiniteDuration] =
   *      | Invariant[Semigroup].imap(Semigroup[Long])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroup.combine(2.seconds, 3.seconds)
   * res1: FiniteDuration = 5 seconds
   * }}}
   */
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  /**
   * Compose Invariant `F[_]` and `G[_]` then produce `Invariant[F[G[_]]]` using their `imap`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.concurrent.duration._
   *
   * scala> val durSemigroupList: Semigroup[List[FiniteDuration]] =
   *      | Invariant[Semigroup].compose[List].imap(Semigroup[List[Long]])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroupList.combine(List(2.seconds, 3.seconds), List(4.seconds))
   * res1: List[FiniteDuration] = List(2 seconds, 3 seconds, 4 seconds)
   * }}}
   */
  def compose[G[_]: Invariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariant[F, G] {
      val F: Invariant[F] = self
      val G: Invariant[G] = Invariant[G]
    }

  /**
   * Compose Invariant `F[_]` and Functor `G[_]` then produce `Invariant[F[G[_]]]`
   * using F's `imap` and G's `map`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.concurrent.duration._
   *
   * scala> val durSemigroupList: Semigroup[List[FiniteDuration]] =
   *      | Invariant[Semigroup]
   *      |   .composeFunctor[List]
   *      |   .imap(Semigroup[List[Long]])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroupList.combine(List(2.seconds, 3.seconds), List(4.seconds))
   * res1: List[FiniteDuration] = List(2 seconds, 3 seconds, 4 seconds)
   * }}}
   */
  def composeFunctor[G[_]: Functor]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantCovariant[F, G] {
      val F: Invariant[F] = self
      val G: Functor[G] = Functor[G]
    }

  /**
   * Compose Invariant `F[_]` and Contravariant `G[_]` then produce `Invariant[F[G[_]]]`
   * using F's `imap` and G's `contramap`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import scala.concurrent.duration._
   *
   * scala> type ToInt[T] = T => Int
   * scala> val durSemigroupToInt: Semigroup[ToInt[FiniteDuration]] =
   *      | Invariant[Semigroup]
   *      |   .composeContravariant[ToInt]
   *      |   .imap(Semigroup[ToInt[Long]])(Duration.fromNanos)(_.toNanos)
   * // semantically equal to (2.seconds.toSeconds.toInt + 1) + (2.seconds.toSeconds.toInt * 2) = 7
   * scala> durSemigroupToInt.combine(_.toSeconds.toInt + 1, _.toSeconds.toInt * 2)(2.seconds)
   * res1: Int = 7
   * }}}
   */
  def composeContravariant[G[_]: Contravariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantContravariant[F, G] {
      val F: Invariant[F] = self
      val G: Contravariant[G] = Contravariant[G]
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
object Invariant extends ScalaVersionSpecificInvariantInstances with InvariantInstances0 {
  implicit def catsInstancesForId: Distributive[Id] & Bimonad[Id] & CommutativeMonad[Id] & NonEmptyTraverse[Id] =
    cats.catsInstancesForId
  @deprecated("Added for bincompat", "2.8.0")
  @targetName3("catsInstancesForId")
  private[cats] def catsInstancesForIdCompat2_6_1: Comonad[Id] =
    cats.catsInstancesForId
  implicit def catsMonadErrorForEither[A]: MonadError[Either[A, *], A] =
    cats.instances.either.catsStdInstancesForEither[A]
  implicit def catsInstancesForOption
    : MonadError[Option, Unit] & Alternative[Option] & CoflatMap[Option] & CommutativeMonad[Option] =
    cats.instances.option.catsStdInstancesForOption
  implicit def catsInstancesForList: Monad[List] & Alternative[List] & CoflatMap[List] =
    cats.instances.list.catsStdInstancesForList
  implicit def catsInstancesForVector: Monad[Vector] & Alternative[Vector] & CoflatMap[Vector] =
    cats.instances.vector.catsStdInstancesForVector
  implicit def catsInstancesForQueue: Monad[Queue] & Alternative[Queue] & CoflatMap[Queue] =
    cats.instances.queue.catsStdInstancesForQueue
  implicit def catsMonadForTailRec: Monad[TailRec] = cats.instances.tailRec.catsInstancesForTailRec

  implicit def catsFlatMapForMap[K]: FlatMap[Map[K, *]] = cats.instances.map.catsStdInstancesForMap[K]
  implicit def catsFlatMapForSortedMap[K]: FlatMap[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]
  implicit def catsBimonadForFunction0: Bimonad[Function0] = cats.instances.function.catsStdBimonadForFunction0
  implicit def catsContravariantMonoidalForFunction1[R: Monoid]: ContravariantMonoidal[* => R] =
    cats.instances.function.catsStdContravariantMonoidalForFunction1[R]
  implicit def catsFunctorForPair: Functor[λ[P => (P, P)]] = cats.instances.tuple.catsDataFunctorForPair

  implicit def catsInstancesForTry: MonadThrow[Try] & CoflatMap[Try] =
    cats.instances.try_.catsStdInstancesForTry

  /**
   * @deprecated
   *   Any non-pure use of [[scala.concurrent.Future Future]] with Cats is error prone
   *   (particularly the semantics of [[cats.Traverse#traverse traverse]] with regard to execution order are unspecified).
   *   We recommend using [[https://typelevel.org/cats-effect/ Cats Effect `IO`]] as a replacement for ''every'' use case of [[scala.concurrent.Future Future]].
   *   However, at this time there are no plans to remove these instances from Cats.
   *
   * @see [[https://github.com/typelevel/cats/issues/4176 Changes in Future traverse behavior between 2.6 and 2.7]]
   */
  implicit def catsInstancesForFuture(implicit
    ec: ExecutionContext
  ): MonadThrow[Future] & CoflatMap[Future] =
    cats.instances.future.catsStdInstancesForFuture(ec)

  implicit def catsContravariantMonoidalForOrder: ContravariantMonoidal[Order] =
    cats.instances.order.catsContravariantMonoidalForOrder
  implicit def catsContravariantMonoidalForPartialOrder: ContravariantMonoidal[PartialOrder] =
    cats.instances.partialOrder.catsContravariantMonoidalForPartialOrder
  implicit def catsContravariantMonoidalForOrdering: ContravariantMonoidal[Ordering] =
    cats.instances.ordering.catsContravariantMonoidalForOrdering
  implicit def catsContravariantMonoidalForPartialOrdering: ContravariantMonoidal[PartialOrdering] =
    cats.instances.partialOrdering.catsContravariantMonoidalForPartialOrdering
  implicit def catsContravariantMonoidalForEq: ContravariantMonoidal[Eq] =
    cats.instances.eq.catsContravariantMonoidalForEq
  implicit def catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] =
    cats.instances.equiv.catsContravariantMonoidalForEquiv
  implicit def catsContravariantForHash: Contravariant[Hash] =
    cats.instances.all.catsContravariantForHash
  implicit def catsInvariantMonoidalForSemigroup: InvariantMonoidal[Semigroup] =
    cats.instances.invariant.catsInvariantMonoidalSemigroup
  implicit def catsInvariantMonoidalForCommutativeSemigroup: InvariantMonoidal[CommutativeSemigroup] =
    cats.instances.invariant.catsInvariantMonoidalCommutativeSemigroup
  implicit def catsInvariantSemigroupalForMonoid: InvariantSemigroupal[Monoid] =
    cats.instances.invariant.catsSemigroupalForMonoid
  implicit def catsInvariantForNumeric: Invariant[Numeric] =
    cats.instances.invariant.catsInvariantForNumeric
  implicit def catsInvariantForIntegral: Invariant[Integral] =
    cats.instances.invariant.catsInvariantForIntegral
  implicit def catsInvariantForFractional: Invariant[Fractional] =
    cats.instances.invariant.catsInvariantForFractional

  implicit val catsInvariantMonoid: Invariant[Monoid] = new Invariant[Monoid] {

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] =
      new Monoid[B] {
        val empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  implicit val catsInvariantBand: Invariant[Band] = new Invariant[Band] {

    def imap[A, B](fa: Band[A])(f: A => B)(g: B => A): Band[B] =
      new Band[B] {
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }
  }

  implicit val catsInvariantSemilattice: Invariant[Semilattice] = new Invariant[Semilattice] {

    def imap[A, B](fa: Semilattice[A])(f: A => B)(g: B => A): Semilattice[B] =
      new Semilattice[B] {
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  implicit val catsInvariantCommutativeMonoid: Invariant[CommutativeMonoid] = new Invariant[CommutativeMonoid] {

    def imap[A, B](fa: CommutativeMonoid[A])(f: A => B)(g: B => A): CommutativeMonoid[B] =
      new CommutativeMonoid[B] {
        val empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  implicit val catsInvariantBoundedSemilattice: Invariant[BoundedSemilattice] = new Invariant[BoundedSemilattice] {

    def imap[A, B](fa: BoundedSemilattice[A])(f: A => B)(g: B => A): BoundedSemilattice[B] =
      new BoundedSemilattice[B] {
        val empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  implicit val catsInvariantGroup: Invariant[Group] = new Invariant[Group] {

    def imap[A, B](fa: Group[A])(f: A => B)(g: B => A): Group[B] =
      new Group[B] {
        val empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        def inverse(b: B): B = f(fa.inverse(g(b)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  implicit val catsInvariantCommutativeGroup: Invariant[CommutativeGroup] = new Invariant[CommutativeGroup] {

    def imap[A, B](fa: CommutativeGroup[A])(f: A => B)(g: B => A): CommutativeGroup[B] =
      new CommutativeGroup[B] {
        val empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        def inverse(b: B): B = f(fa.inverse(g(b)))
        override def combineAllOption(bs: IterableOnce[B]): Option[B] =
          fa.combineAllOption(bs.iterator.map(g)).map(f)
      }

  }

  @deprecated("Use catsStdInstancesForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsComonadForTuple2[A]: Comonad[(A, *)] = cats.instances.tuple.catsStdInstancesForTuple2[A]

  /**
   * Summon an instance of [[Invariant]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Invariant[F]): Invariant[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantOps[F[_], A](target: F[A])(implicit tc: Invariant[F]): AllOps[F, A] {
      type TypeClassType = Invariant[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Invariant[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Invariant[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def imap[B](f: A => B)(g: B => A): F[B] = typeClassInstance.imap[A, B](self)(f)(g)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToInvariantOps extends Serializable {
    implicit def toInvariantOps[F[_], A](target: F[A])(implicit tc: Invariant[F]): Ops[F, A] {
      type TypeClassType = Invariant[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Invariant[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantOps

}

private[cats] trait InvariantInstances0 extends InvariantInstances1 {
  implicit def catsContravariantForFunction1[R]: Contravariant[* => R] =
    cats.instances.function.catsStdContravariantForFunction1[R]
  implicit def catsDistributiveForFunction0: Distributive[Function0] = cats.instances.function.function0Distributive
  implicit def catsDistributiveForFunction1[I]: Distributive[I => *] =
    cats.instances.function.catsStdDistributiveForFunction1[I]

  @deprecated("Use catsStdCommutativeMonadForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsCommutativeMonadForTuple2[X](implicit X: CommutativeMonoid[X]): CommutativeMonad[(X, *)] =
    cats.instances.tuple.catsStdCommutativeMonadForTuple2[X]
}

private trait InvariantInstances1 extends InvariantInstances2 {
  implicit def catsMonadForFunction1[I]: Monad[I => *] = cats.instances.function.catsStdMonadForFunction1[I]
}

private[cats] trait InvariantInstances2 extends cats.instances.NTupleMonadInstances with TupleInstances0 {
  implicit def catsApplicativeForArrow[F[_, _], A](implicit F: Arrow[F]): Applicative[F[A, *]] =
    new ArrowApplicative[F, A](F)
  implicit def catsInstancesForSeq: Monad[Seq] & Alternative[Seq] & CoflatMap[Seq] =
    cats.instances.seq.catsStdInstancesForSeq
}

private[cats] trait TupleInstances0 extends TupleInstances1 {
  @deprecated("Use catsStdCommutativeFlatMapForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsCommutativeFlatMapForTuple2[X](implicit X: CommutativeSemigroup[X]): CommutativeFlatMap[(X, *)] =
    cats.instances.tuple.catsStdCommutativeFlatMapForTuple2[X]
}

private trait TupleInstances1 extends TupleInstances2 {
  @deprecated("Use catsStdMonadForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsMonadForTuple2[X](implicit X: Monoid[X]): Monad[(X, *)] =
    cats.instances.tuple.catsStdMonadForTuple2[X]
}

private trait TupleInstances2 {
  @deprecated("Use catsStdFlatMapForTuple2 on cats.instances.NTupleMonadInstances", "2.4.0")
  def catsFlatMapForTuple2[X](implicit X: Semigroup[X]): FlatMap[(X, *)] =
    cats.instances.tuple.catsStdFlatMapForTuple2[X]
}
