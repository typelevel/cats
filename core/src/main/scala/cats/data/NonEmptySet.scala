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
package data

import cats.kernel._

import scala.collection.immutable._

import kernel.compat.scalaVersionSpecific._

/**
 * Actual implementation for [[cats.data.NonEmptySet]]
 *
 * @note This object is kept public for the sake of binary compatibility only
 *       and therefore is subject to changes in future versions of Cats.
 *       Do not use directly - use [[cats.data.NonEmptySet]] instead.
 */
object NonEmptySetImpl extends NonEmptySetInstances with Newtype {

  private[data] def create[A](s: SortedSet[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[data] def unwrap[A](s: Type[A]): SortedSet[A] =
    s.asInstanceOf[SortedSet[A]]

  def fromSet[A](as: SortedSet[A]): Option[NonEmptySet[A]] =
    if (as.nonEmpty) Option(create(as)) else None

  def fromSetUnsafe[A](set: SortedSet[A]): NonEmptySet[A] =
    if (set.nonEmpty) create(set)
    else throw new IllegalArgumentException("Cannot create NonEmptySet from empty set")

  def of[A](a: A, as: A*)(implicit A: Order[A]): NonEmptySet[A] =
    create(SortedSet(a +: as: _*)(A.toOrdering))

  def apply[A](head: A, tail: SortedSet[A])(implicit A: Order[A]): NonEmptySet[A] =
    create(SortedSet(head)(A.toOrdering) ++ tail)
  def one[A](a: A)(implicit A: Order[A]): NonEmptySet[A] = create(SortedSet(a)(A.toOrdering))

  implicit def catsNonEmptySetOps[A](value: NonEmptySet[A]): NonEmptySetOps[A] =
    new NonEmptySetOps(value)
}

@suppressUnusedImportWarningForScalaVersionSpecific
sealed class NonEmptySetOps[A](val value: NonEmptySet[A]) {

  implicit private val ordering: Ordering[A] = toSortedSet.ordering
  implicit private val order: Order[A] = Order.fromOrdering

  /**
   * Converts this set to a `SortedSet`
   */
  def toSortedSet: SortedSet[A] = NonEmptySetImpl.unwrap(value)

  /**
   * Adds an element to this set, returning a new `NonEmptySet`
   */
  def add(a: A): NonEmptySet[A] = NonEmptySet.create(toSortedSet + a)

  /**
   * Alias for [[union]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 4, 5)
   * scala> nes ++ NonEmptySet.of(1, 2, 7)
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 4, 5, 7)
   * }}}
   */
  def ++(as: NonEmptySet[A]): NonEmptySet[A] = union(as)

  /**
   * Alias for [[union]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 4, 5)
   * scala> nes | NonEmptySet.of(1, 2, 7)
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 4, 5, 7)
   * }}}
   */
  def |(as: NonEmptySet[A]): NonEmptySet[A] = union(as)

  /**
   * Alias for [[diff]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 4, 5)
   * scala> nes -- NonEmptySet.of(1, 2, 7)
   * res0: scala.collection.immutable.SortedSet[Int] = TreeSet(4, 5)
   * }}}
   */
  def --(as: NonEmptySet[A]): SortedSet[A] = diff(as)

  /**
   * Alias for [[diff]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 4, 5)
   * scala> nes &~ NonEmptySet.of(1, 2, 7)
   * res0: scala.collection.immutable.SortedSet[Int] = TreeSet(4, 5)
   * }}}
   */
  def &~(as: NonEmptySet[A]): SortedSet[A] = diff(as)

  /**
   * Alias for [[intersect]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 4, 5)
   * scala> nes & NonEmptySet.of(1, 2, 7)
   * res0: scala.collection.immutable.SortedSet[Int] = TreeSet(1, 2)
   * }}}
   */
  def &(as: NonEmptySet[A]): SortedSet[A] = intersect(as)

  /**
   * Removes a key from this set, returning a new `SortedSet`.
   */
  def -(a: A): SortedSet[A] = toSortedSet - a

  /**
   * Applies f to all the elements
   */
  def map[B](f: A => B)(implicit B: Order[B]): NonEmptySet[B] = {
    implicit val bOrdering: Ordering[B] = B.toOrdering
    NonEmptySetImpl.create(toSortedSet.map(f))
  }

  /**
   * Converts this set to a `NonEmptyList`.
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 3, 4, 5)
   * scala> nes.toNonEmptyList
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
   * }}}
   */
  def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromListUnsafe(toSortedSet.toList)

  /**
   * Returns the first element of this set.
   */
  def head: A = toSortedSet.head

  /**
   * Returns all but the first element of this set.
   */
  def tail: SortedSet[A] = toSortedSet.tail

  /**
   * Returns the last element of this set.
   */
  def last: A = toSortedSet.last

  /**
   * Alias for [[contains]]
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 3, 4, 5)
   * scala> nes(3)
   * res0: Boolean = true
   * scala> nes(7)
   * res1: Boolean = false
   * }}}
   */
  def apply(a: A): Boolean = contains(a)

  /**
   * Tests if some element is contained in this set.
   */
  def contains(a: A): Boolean = toSortedSet(a)

  /**
   * Computes the difference of this set and another set.
   */
  def diff(as: NonEmptySet[A]): SortedSet[A] = toSortedSet -- as.toSortedSet

  /**
   * Computes the union between this NES and another NES.
   */
  def union(as: NonEmptySet[A]): NonEmptySet[A] = NonEmptySetImpl.create(toSortedSet ++ as.toSortedSet)

  /**
   * Computes the intersection between this set and another set.
   */
  def intersect(as: NonEmptySet[A]): SortedSet[A] = toSortedSet.filter(as.apply)

  /**
   * Tests whether a predicate holds for all elements of this set.
   */
  def forall(p: A => Boolean): Boolean = toSortedSet.forall(p)

  /**
   * Tests whether a predicate holds for at least one element of this set.
   */
  def exists(f: A => Boolean): Boolean = toSortedSet.exists(f)

  /**
   * Returns the first value that matches the given predicate.
   */
  def find(f: A => Boolean): Option[A] = toSortedSet.find(f)

  /**
   * Returns a new `SortedSet` containing all elements where the result of `pf` is defined.
   */
  def collect[B](pf: PartialFunction[A, B])(implicit B: Order[B]): SortedSet[B] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toSortedSet.collect(pf)
  }

  /**
   * Filters all elements of this set that do not satisfy the given predicate.
   */
  def filter(p: A => Boolean): SortedSet[A] = toSortedSet.filter(p)

  /**
   * Filters all elements of this set that satisfy the given predicate.
   */
  def filterNot(p: A => Boolean): SortedSet[A] = filter(t => !p(t))

  /**
   * Left-associative fold using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    toSortedSet.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[SortedSet].foldRight(toSortedSet, lb)(f)

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft(f: (A, A) => A): A =
    toSortedSet.reduceLeft(f)

  /**
   * Apply `f` to the "initial element" of this set and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B =
    tail.foldLeft(f(head))((b, a) => g(b, a))

  /**
   * Left-associative reduce using f.
   */
  def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(identity)(f)

  /**
   * Apply `f` to the "initial element" of this set and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always((head, tail)).flatMap { case (a, ga) =>
      Foldable[SortedSet].reduceRightToOption(ga)(f)(g).flatMap {
        case Some(b) => g(a, Now(b))
        case None    => Later(f(a))
      }
    }

  /**
   * Reduce using the Semigroup of A
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toSortedSet).get

  /**
   * Map a function over all the elements of this set and concatenate the resulting sets into one.
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val nes = NonEmptySet.of(1, 2, 3)
   * scala> nes.concatMap(n => NonEmptySet.of(n, n * 4, n * 5))
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4, 5, 8, 10, 12, 15)
   * }}}
   */
  def concatMap[B](f: A => NonEmptySet[B])(implicit B: Order[B]): NonEmptySet[B] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    NonEmptySetImpl.create(toSortedSet.flatMap(a => f(a).toSortedSet))
  }

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show(implicit A: Show[A]): String =
    s"NonEmpty${Show[SortedSet[A]].show(toSortedSet)}"

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptySet[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===(that: NonEmptySet[A]): Boolean =
    Eq[SortedSet[A]].eqv(toSortedSet, that.toSortedSet)

  /**
   * Returns the number of elements in this set.
   */
  def length: Int = toSortedSet.size

  /**
   * Zips this `NonEmptySet` with another `NonEmptySet` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptySet
   * scala> import cats.implicits._
   * scala> val as = NonEmptySet.of(1, 2, 3)
   * scala> val bs = NonEmptySet.of("A", "B", "C")
   * scala> as.zipWith(bs)(_.toString + _)
   * res0: cats.data.NonEmptySet[String] = TreeSet(1A, 2B, 3C)
   * }}}
   */
  def zipWith[B, C](b: NonEmptySet[B])(f: (A, B) => C)(implicit C: Order[C]): NonEmptySet[C] = {
    implicit val cOrdering: Ordering[C] = C.toOrdering
    NonEmptySetImpl.create(toSortedSet.lazyZip(b.toSortedSet).map(f))
  }

  /**
   * Zips this `NonEmptySet` with its index.
   */
  def zipWithIndex: NonEmptySet[(A, Int)] =
    NonEmptySetImpl.create(cats.compat.SortedSet.zipWithIndex(toSortedSet))

  /**
   * Groups elements inside this `NonEmptySet` according to the `Order`
   * of the keys produced by the given mapping function.
   */
  def groupBy[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptySet[A]] =
    reduceLeftTo(a => NonEmptyMap.one(f(a), NonEmptySet.one(a))) { (acc, a) =>
      val key = f(a)
      val result = acc.lookup(key) match {
        case Some(nes) => nes.add(a)
        case _         => NonEmptySet.one(a)
      }
      acc.add((key, result))
    }
}

sealed abstract private[data] class NonEmptySetInstances extends NonEmptySetInstances0 {
  implicit val catsDataInstancesForNonEmptySet: SemigroupK[NonEmptySet] with Reducible[NonEmptySet] =
    new SemigroupK[NonEmptySet] with Reducible[NonEmptySet] {

      def combineK[A](a: NonEmptySet[A], b: NonEmptySet[A]): NonEmptySet[A] =
        a | b

      override def size[A](fa: NonEmptySet[A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptySet[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptySet[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptySet[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptySet[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.reduceRightTo(f)(g)

      override def foldLeft[A, B](fa: NonEmptySet[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptySet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptySet[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toSortedSet.iterator.map(f))

      override def fold[A](fa: NonEmptySet[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptySet[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptySet[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptySet[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptySet[A]): List[A] = fa.toSortedSet.toList

      override def toIterable[A](fa: NonEmptySet[A]): Iterable[A] = fa.toSortedSet

      override def toNonEmptyList[A](fa: NonEmptySet[A]): NonEmptyList[A] =
        fa.toNonEmptyList
    }

  implicit def catsDataOrderForNonEmptySet[A](implicit A: Order[A]): Order[NonEmptySet[A]] =
    new NonEmptySetOrder[A] {
      implicit override def A0: Order[A] = A
    }

  implicit def catsDataShowForNonEmptySet[A: Show]: Show[NonEmptySet[A]] = _.show

  implicit def catsDataSemilatticeForNonEmptySet[A]: Semilattice[NonEmptySet[A]] =
    new Semilattice[NonEmptySet[A]] {
      def combine(x: NonEmptySet[A], y: NonEmptySet[A]): NonEmptySet[A] = x | y
    }
}

sealed abstract private[data] class NonEmptySetInstances0 extends NonEmptySetInstances1 {
  implicit def catsDataHashForNonEmptySet[A: Order: Hash]: Hash[NonEmptySet[A]] =
    Hash[SortedSet[A]].asInstanceOf[Hash[NonEmptySet[A]]]
}

sealed abstract private[data] class NonEmptySetInstances1 {
  @deprecated("use catsDataEqForNonEmptySetFromEqA instead", "2.8.0")
  def catsDataEqForNonEmptySet[A](implicit A: Order[A]): Eq[NonEmptySet[A]] =
    catsDataEqForNonEmptySetFromEqA[A]

  implicit def catsDataEqForNonEmptySetFromEqA[A](implicit A: Eq[A]): Eq[NonEmptySet[A]] =
    new NonEmptySetEq[A] {
      implicit override def A0: Eq[A] = A
    }
}

sealed abstract private[data] class NonEmptySetOrder[A] extends Order[NonEmptySet[A]] with NonEmptySetEq[A] {
  implicit override def A0: Order[A]

  override def compare(x: NonEmptySet[A], y: NonEmptySet[A]): Int =
    Order[SortedSet[A]].compare(x.toSortedSet, y.toSortedSet)
}

sealed private[data] trait NonEmptySetEq[A] extends Eq[NonEmptySet[A]] {
  implicit def A0: Eq[A]

  override def eqv(x: NonEmptySet[A], y: NonEmptySet[A]): Boolean = x === y
}
