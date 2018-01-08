/*
 * Copyright (c) 2018 Luka Jacobowitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats
package data

import cats.instances.sortedSet._
import cats.kernel._
import cats.{Always, Eq, Eval, Foldable, Later, Now, Reducible, SemigroupK, Show}

import scala.collection.immutable._

final class NonEmptySet[A] private (val set: SortedSet[A]) extends AnyVal {

  private implicit def ordering: Ordering[A] = set.ordering
  private implicit def order: Order[A] = Order.fromOrdering

  def +(a: A): NonEmptySet[A] = new NonEmptySet(set + a)
  def ++(as: NonEmptySet[A]): NonEmptySet[A] = concat(as)

  def concat(as: NonEmptySet[A]): NonEmptySet[A] = new NonEmptySet(set ++ as.set)

  def -(a: A): SortedSet[A] = set - a
  def map[B: Order](f: A ⇒ B): NonEmptySet[B] =
    new NonEmptySet(SortedSet(set.map(f).to: _*)(Order[B].toOrdering))

  def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromListUnsafe(set.toList)

  def head: A = set.head
  def tail: SortedSet[A] = set.tail
  def last: A = set.last

  def apply(a: A): Boolean = set(a)
  def contains(a: A): Boolean = set.contains(a)


  def union(as: NonEmptySet[A]): NonEmptySet[A] = new NonEmptySet(set.union(as.toSet))
  def size: Int = set.size
  def forall(p: A ⇒ Boolean): Boolean = set.forall(p)
  def exists(f: A ⇒ Boolean): Boolean = set.exists(f)
  def find(f: A ⇒ Boolean): Option[A] = set.find(f)
  def collect[B](pf: PartialFunction[A, B]): Set[B] = set.collect(pf)
  def filter(p: A ⇒ Boolean): SortedSet[A] = set.filter(p)
  def filterNot(p: A ⇒ Boolean): SortedSet[A] = filter(t => !p(t))


  /**
    * Left-associative fold using f.
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    set.foldLeft(b)(f)

  /**
    * Right-associative fold using f.
    */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[SortedSet].foldRight(set, lb)(f)

  /**
    * Left-associative reduce using f.
    */
  def reduceLeft(f: (A, A) => A): A =
    set.reduceLeft(f)

  def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    tail.foldLeft(f(head))((b, a) => g(b, a))
  }

  /**
    * Left-associative reduce using f.
    */
  def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(identity)(f)

  def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always((head, tail)).flatMap { case (a, ga) =>
      Foldable[SortedSet].reduceRightToOption(ga)(f)(g).flatMap {
        case Some(b) => g(a, Now(b))
        case None => Later(f(a))
      }
    }

  /**
    * Reduce using the Semigroup of A
    */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(set).get

  def toSet: SortedSet[A] = set

  /**
    * Typesafe stringification method.
    *
    * This method is similar to .toString except that it stringifies
    * values according to Show[_] instances, rather than using the
    * universal .toString method.
    */
  def show(implicit A: Show[A]): String =
    s"NonEmpty${Show[SortedSet[A]].show(set)}"

  /**
    * Typesafe equality operator.
    *
    * This method is similar to == except that it only allows two
    * NonEmptySet[A] values to be compared to each other, and uses
    * equality provided by Eq[_] instances, rather than using the
    * universal equality provided by .equals.
    */
  def ===(that: NonEmptySet[A]): Boolean =
    Eq[SortedSet[A]].eqv(set, that.toSet)

  def length: Int = size

  override def toString: String = s"NonEmpty${set.toString}"

  /**
    * Zips this `NonEmptySet` with another `NonEmptySet` and applies a function for each pair of elements.
    *
    * {{{
    * scala> import cats.data.NonEmptySet
    * scala> import cats.implicits._
    * scala> val as = NonEmptySet.of(1, 2, 3)
    * scala> val bs = NonEmptySet.of("A", "B", "C")
    * scala> as.zipWith(bs)(_ + _)
    * res0: cats.data.NonEmptySet[String] = NonEmptyTreeSet(1A, 2B, 3C)
    * }}}
    */
  def zipWith[B, C: Order](b: NonEmptySet[B])(f: (A, B) => C): NonEmptySet[C] =
    new NonEmptySet(SortedSet((set, b.toSet).zipped.map(f).to: _*)(Order[C].toOrdering))

  def zipWithIndex: NonEmptySet[(A, Int)] =
    new NonEmptySet(set.zipWithIndex)
}

private[data] sealed abstract class NonEmptySetInstances {
  implicit val catsDataInstancesForNonEmptySet: SemigroupK[NonEmptySet] with Reducible[NonEmptySet] =
    new SemigroupK[NonEmptySet] with Reducible[NonEmptySet] {

      def combineK[A](a: NonEmptySet[A], b: NonEmptySet[A]): NonEmptySet[A] =
        a ++ b

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
        B.combineAll(fa.toSet.iterator.map(f))

      override def fold[A](fa: NonEmptySet[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptySet[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptySet[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptySet[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptySet[A]): List[A] = fa.toSet.toList

      override def toNonEmptyList[A](fa: NonEmptySet[A]): NonEmptyList[A] =
        NonEmptyList(fa.head, fa.tail.toList)
    }

  implicit def catsDataEqForNonEmptySet[A: Order]: Eq[NonEmptySet[A]] =
    new Eq[NonEmptySet[A]]{
      def eqv(x: NonEmptySet[A], y: NonEmptySet[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptySet[A](implicit A: Show[A]): Show[NonEmptySet[A]] =
    Show.show[NonEmptySet[A]](_.show)

  implicit def catsDataSemilatticeForNonEmptySet[A]: Semilattice[NonEmptySet[A]] = new Semilattice[NonEmptySet[A]] {
    def combine(x: NonEmptySet[A], y: NonEmptySet[A]): NonEmptySet[A] = x ++ y
  }
}

object NonEmptySet extends NonEmptySetInstances {
  def fromSet[A: Order](as: SortedSet[A]): Option[NonEmptySet[A]] =
    if (as.nonEmpty) Option(new NonEmptySet(as)) else None

  def fromSetUnsafe[A: Order](set: SortedSet[A]): NonEmptySet[A] =
    if (set.nonEmpty) new NonEmptySet(set)
    else throw new IllegalArgumentException("Cannot create NonEmptySet from empty set")


  def of[A: Order](a: A, as: A*): NonEmptySet[A] =
    new NonEmptySet(SortedSet(a)(Order[A].toOrdering) ++ SortedSet(as: _*)(Order[A].toOrdering) + a)
  def apply[A: Order](head: A, tail: SortedSet[A]): NonEmptySet[A] = new NonEmptySet(SortedSet(head)(Order[A].toOrdering) ++ tail)
  def one[A: Order](a: A): NonEmptySet[A] = new NonEmptySet(SortedSet(a)(Order[A].toOrdering))
}
