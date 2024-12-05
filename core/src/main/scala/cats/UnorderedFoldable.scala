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

import cats.kernel.CommutativeMonoid
import scala.collection.immutable.{Queue, Seq, SortedMap, SortedSet}
import scala.util.Try

/**
 * `UnorderedFoldable` is like a `Foldable` for unordered containers.
 */
trait UnorderedFoldable[F[_]] extends Serializable {

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  /**
   * Fold in a [[CommutativeApplicative]] context by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `CommutativeMonoid[B]` instance.
   *
   * {{{
   * scala> import cats.UnorderedFoldable
   * scala> import cats.syntax.all._
   * scala> val evenNumbers = Set(2,4,6,8,10)
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> UnorderedFoldable[Set].unorderedFoldMapA(evenNumbers)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> UnorderedFoldable[Set].unorderedFoldMapA(evenNumbers + 11)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def unorderedFoldMapA[G[_], A, B](fa: F[A])(
    f: A => G[B]
  )(implicit G: CommutativeApplicative[G], B: CommutativeMonoid[B]): G[B] =
    unorderedFoldMap(fa)(f)(CommutativeApplicative.commutativeMonoidFor)

  /**
   * Tests if `fa` contains `v` using the `Eq` instance for `A`
   */
  def contains_[A](fa: F[A], v: A)(implicit ev: Eq[A]): Boolean =
    exists(fa)(a => ev.eqv(a, v))

  /**
   * Returns true if there are no elements. Otherwise false.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    !nonEmpty(fa)

  def nonEmpty[A](fa: F[A]): Boolean =
    exists(fa)(Function.const(true))

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.orEvalMonoid).value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.andEvalMonoid).value

  /**
   * The size of this UnorderedFoldable.
   *
   * This is overridden in structures that have more efficient size implementations
   * (e.g. Vector, Set, Map).
   *
   * Note: will not terminate for infinite-sized collections.
   */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1L)

  /**
   * Count the number of elements in the structure that satisfy the given predicate.
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val map1 = Map[Int, String]()
   * scala> val p1: String => Boolean = _.length > 0
   * scala> UnorderedFoldable[Map[Int, *]].count(map1)(p1)
   * res0: Long = 0
   *
   * scala> val map2 = Map(1 -> "hello", 2 -> "world", 3 -> "!")
   * scala> val p2: String => Boolean = _.length > 1
   * scala> UnorderedFoldable[Map[Int, *]].count(map2)(p2)
   * res1: Long = 2
   * }}}
   */

  def count[A](fa: F[A])(p: A => Boolean): Long =
    unorderedFoldMap(fa)(a => if (p(a)) 1L else 0L)
}

private[cats] trait UnorderedFoldableLowPriority {
  implicit def catsTraverseForSeq: Traverse[Seq] = cats.instances.seq.catsStdInstancesForSeq
}

object UnorderedFoldable
    extends ScalaVersionSpecificTraverseInstances
    with cats.instances.NTupleUnorderedFoldableInstances
    with UnorderedFoldableLowPriority {

  private val orEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.False

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => Eval.True
        case false => ly
      }
  }

  private val andEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.True

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => ly
        case false => Eval.False
      }
  }

  implicit def catsNonEmptyTraverseForId: NonEmptyTraverse[Id] = catsInstancesForId
  implicit def catsTraverseForOption: Traverse[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsTraverseForList: Traverse[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsTraverseForVector: Traverse[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsTraverseForQueue: Traverse[Queue] = cats.instances.queue.catsStdInstancesForQueue
  implicit def catsUnorderedTraverseForSet: UnorderedTraverse[Set] = cats.instances.set.catsStdInstancesForSet
  implicit def catsFoldableForSortedSet: Foldable[SortedSet] = cats.instances.sortedSet.catsStdInstancesForSortedSet
  implicit def catsTraverseForSortedMap[K]: Traverse[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]

  implicit def catsUnorderedTraverseForMap[K]: UnorderedTraverse[Map[K, *]] =
    cats.instances.map.catsStdInstancesForMap[K]

  implicit def catsTraverseForEither[A]: Traverse[Either[A, *]] = cats.instances.either.catsStdInstancesForEither[A]
  implicit def catsTraverseForTry: Traverse[Try] = cats.instances.try_.catsStdInstancesForTry

  @deprecated("Use catsStdInstancesForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsInstancesForTuple[A]: Traverse[(A, *)] & Reducible[(A, *)] =
    cats.instances.tuple.catsStdInstancesForTuple2[A]

  /**
   * Summon an instance of [[UnorderedFoldable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: UnorderedFoldable[F]): UnorderedFoldable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllUnorderedFoldableOps[F[_], A](target: F[A])(implicit tc: UnorderedFoldable[F]): AllOps[F, A] {
      type TypeClassType = UnorderedFoldable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = UnorderedFoldable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: UnorderedFoldable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def unorderedFoldMap[B](f: A => B)(implicit ev$1: CommutativeMonoid[B]): B =
      typeClassInstance.unorderedFoldMap[A, B](self)(f)
    def unorderedFold(implicit ev$1: CommutativeMonoid[A]): A = typeClassInstance.unorderedFold[A](self)
    def unorderedFoldMapA[G[_], B](
      f: A => G[B]
    )(implicit ev$1: CommutativeApplicative[G], ev$2: CommutativeMonoid[B]): G[B] =
      typeClassInstance.unorderedFoldMapA[G, A, B](self)(f)
    def isEmpty: Boolean = typeClassInstance.isEmpty[A](self)
    def nonEmpty: Boolean = typeClassInstance.nonEmpty[A](self)
    def exists(p: A => Boolean): Boolean = typeClassInstance.exists[A](self)(p)
    def forall(p: A => Boolean): Boolean = typeClassInstance.forall[A](self)(p)
    def size: Long = typeClassInstance.size[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToUnorderedFoldableOps extends Serializable {
    implicit def toUnorderedFoldableOps[F[_], A](target: F[A])(implicit tc: UnorderedFoldable[F]): Ops[F, A] {
      type TypeClassType = UnorderedFoldable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = UnorderedFoldable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToUnorderedFoldableOps
}
