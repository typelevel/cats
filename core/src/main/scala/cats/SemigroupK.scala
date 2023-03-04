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

import scala.annotation.tailrec
import scala.collection.immutable.{Seq, SortedMap, SortedSet}

import cats.data.Ior
import cats.kernel.compat.scalaVersionSpecific._

/**
 * SemigroupK is a universal semigroup which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type. Thus,
 * SemigroupK is like a Semigroup for kinds (i.e. parametrized
 * types).
 *
 * A SemigroupK[F] can produce a Semigroup[F[A]] for any type A.
 *
 * Here's how to distinguish Semigroup and SemigroupK:
 *
 *  - Semigroup[A] allows two A values to be combined.
 *
 *  - SemigroupK[F] allows two F[A] values to be combined, for any A.
 *    The combination operation just depends on the structure of F,
 *    but not the structure of A.
 */
trait SemigroupK[F[_]] extends Serializable { self =>

  /**
   * Combine two F[A] values.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> SemigroupK[List].combineK(List(1, 2), List(3, 4))
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */

  def combineK[A](x: F[A], y: F[A]): F[A]

  /**
   * Similar to [[combineK]] but uses [[Eval]] to allow for laziness in the second
   * argument. This can allow for "short-circuiting" of computations.
   *
   * NOTE: the default implementation of `combineKEval` does not short-circuit
   * computations. For data structures that can benefit from laziness, [[SemigroupK]]
   * instances should override this method.
   *
   * In the following example, `x.combineK(bomb)` would result in an error,
   * but `combineKEval` "short-circuits" the computation. `x` is `Some` and thus the
   * result of `bomb` doesn't even need to be evaluated in order to determine
   * that the result of `combineKEval` should be `x`.
   *
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.syntax.all._
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = Some(42)
   * scala> x.combineKEval(bomb).value
   * res0: Option[Int] = Some(42)
   * }}}
   */
  def combineKEval[A](x: F[A], y: Eval[F[A]]): Eval[F[A]] =
    y.map(yy => combineK(x, yy))

  /**
   * Given a type A, create a concrete `Semigroup[F[A]]`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val s: Semigroup[List[Int]] = SemigroupK[List].algebra[Int]
   * }}}
   */
  def algebra[A]: Semigroup[F[A]] = combineK(_, _)

  /**
   * "Compose" with a `G[_]` type to form a `SemigroupK` for `λ[α => F[G[α]]]`.
   * Note that this universally works for any `G`, because the "inner" structure
   * isn't considered when combining two instances.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> type ListOption[A] = List[Option[A]]
   * scala> val s: SemigroupK[ListOption] = SemigroupK[List].compose[Option]
   * scala> s.combineK(List(Some(1), None, Some(2)), List(Some(3), None))
   * res0: List[Option[Int]] = List(Some(1), None, Some(2), Some(3), None)
   * }}}
   */
  def compose[G[_]]: SemigroupK[λ[α => F[G[α]]]] =
    new ComposedSemigroupK[F, G] {
      val F = self
    }

  /**
   * Combines `F[A]` and `F[B]` into a `F[Either[A,B]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.SemigroupK
   * scala> import cats.data.NonEmptyList
   * scala> SemigroupK[NonEmptyList].sum(NonEmptyList.one("abc"), NonEmptyList.one(2))
   * res0: NonEmptyList[Either[String,Int]] = NonEmptyList(Left(abc), Right(2))
   * }}}
   */
  def sum[A, B](fa: F[A], fb: F[B])(implicit F: Functor[F]): F[Either[A, B]] =
    combineK(F.map(fa)(Left(_)), F.map(fb)(Right(_)))

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> SemigroupK[List].combineNK(List(1), 5)
   * res0: List[Int] = List(1, 1, 1, 1, 1)
   *
   * }}}
   */
  def combineNK[A](a: F[A], n: Int): F[A] =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for semigroupKs must have n > 0")
    else repeatedCombineNK(a, n)

  /**
   * Return `a` combined with itself more than once.
   */
  protected[this] def repeatedCombineNK[A](a: F[A], n: Int): F[A] = {
    @tailrec def loop(b: F[A], k: Int, extra: F[A]): F[A] =
      if (k == 1) combineK(b, extra)
      else {
        val x = if ((k & 1) == 1) combineK(b, extra) else extra
        loop(combineK(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   *
   * Example:
   * {{{
   * scala> SemigroupK[List].combineAllOptionK(List(List("One"), List("Two"), List("Three")))
   * res0: Option[List[String]] = Some(List(One, Two, Three))
   *
   * scala> SemigroupK[List].combineAllOptionK[String](List.empty)
   * res1: Option[List[String]] = None
   * }}}
   */
  def combineAllOptionK[A](as: IterableOnce[F[A]]): Option[F[A]] =
    as.iterator.reduceOption(combineK[A])

  /**
   * return a semigroupK that reverses the order
   * so combineK(a, b) == reverse.combineK(b, a)
   */
  def reverse: SemigroupK[F] =
    new SemigroupK[F] {
      def combineK[A](a: F[A], b: F[A]): F[A] = self.combineK(b, a)
      // a + a + a + ... is the same when reversed
      override def combineNK[A](a: F[A], n: Int): F[A] = self.combineNK(a, n)
      override def reverse = self
    }

}

@suppressUnusedImportWarningForScalaVersionSpecific
object SemigroupK extends ScalaVersionSpecificMonoidKInstances with SemigroupKInstances0 {
  def align[F[_]: SemigroupK: Functor]: Align[F] =
    new Align[F] {
      def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]] =
        SemigroupK[F].combineK(Functor[F].map(fa)(Ior.left), Functor[F].map(fb)(Ior.right))
      def functor: Functor[F] = Functor[F]
    }

  implicit def catsMonoidKForOption: MonoidK[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsMonoidKForList: MonoidK[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsMonoidKForVector: MonoidK[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsMonoidKForSet: MonoidK[Set] = cats.instances.set.catsStdInstancesForSet
  implicit def catsMonoidKForMap[K]: MonoidK[Map[K, *]] = cats.instances.map.catsStdMonoidKForMap[K]
  implicit def catsSemigroupKForEither[A]: SemigroupK[Either[A, *]] =
    cats.instances.either.catsStdSemigroupKForEither[A]
  implicit def catsSemigroupKForSortedSet: SemigroupK[SortedSet] = cats.instances.sortedSet.catsStdInstancesForSortedSet
  implicit def catsSemigroupKForSortedMap[K]: SemigroupK[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdSemigroupKForSortedMap[K]
  implicit def catsMonoidKForSortedMap[K: Order]: MonoidK[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdMonoidKForSortedMap[K]
  implicit def catsMonoidKForEndo: MonoidK[Endo] = cats.instances.function.catsStdMonoidKForFunction1

  /**
   * Summon an instance of [[SemigroupK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: SemigroupK[F]): SemigroupK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): AllOps[F, A] {
      type TypeClassType = SemigroupK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = SemigroupK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: SemigroupK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def combineK(y: F[A]): F[A] = typeClassInstance.combineK[A](self, y)
    def <+>(y: F[A]): F[A] = typeClassInstance.combineK[A](self, y)
    def combineKEval(y: Eval[F[A]]): Eval[F[A]] = typeClassInstance.combineKEval[A](self, y)
    def sum[B](fb: F[B])(implicit F: Functor[F]): F[Either[A, B]] = typeClassInstance.sum[A, B](self, fb)(F)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToSemigroupKOps extends Serializable {
    implicit def toSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): Ops[F, A] {
      type TypeClassType = SemigroupK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = SemigroupK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToSemigroupKOps

}

trait SemigroupKInstances0 {

  implicit def catsMonoidKForSeq: MonoidK[Seq] = cats.instances.seq.catsStdInstancesForSeq

}
