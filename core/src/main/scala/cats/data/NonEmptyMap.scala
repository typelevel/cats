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

import cats.instances.sortedMap._
import cats.kernel._
import cats.{Always, Apply, Eval, Foldable, Functor, Later, NonEmptyTraverse, Now, SemigroupK, Show}

import scala.collection.immutable._


object NonEmptyMap extends NonEmptyMapInstances {

  type Base
  trait Tag extends Any
  type Type[A, B] <: Base with Tag

  private[data] def create[K, A](m: SortedMap[K, A]): NonEmptyMap[K, A] =
    m.asInstanceOf[NonEmptyMap[K, A]]

  def fromMap[K: Order, A](as: SortedMap[K, A]): Option[NonEmptyMap[K, A]] =
    if (as.nonEmpty) Option(NonEmptyMap.create(as)) else None

  def fromMapUnsafe[K: Order, A](m: SortedMap[K, A]): NonEmptyMap[K, A] =
    if (m.nonEmpty) NonEmptyMap.create(m)
    else throw new IllegalArgumentException("Cannot create NonEmptyMap from empty map")

  def apply[K: Order, A](head: (K, A), tail: SortedMap[K, A]): NonEmptyMap[K, A] =
    NonEmptyMap.create(SortedMap(head)(Order[K].toOrdering) ++ tail)


  def of[K: Order, A](a: (K, A), as: (K, A)*): NonEmptyMap[K, A] =
    NonEmptyMap.create(SortedMap(as: _*)(Order[K].toOrdering) + a)

  def one[K: Order, A](k: K, a: A): NonEmptyMap[K, A] =
    NonEmptyMap.create(SortedMap((k, a))(Order[K].toOrdering))

  implicit class NonEmptyMapOps[K, A](val value: NonEmptyMap.Type[K, A]) extends AnyVal {

    /**
      * Converts this map to a `SortedMap`.
      */
    def toSortedMap: SortedMap[K, A] = value.asInstanceOf[SortedMap[K, A]]

    private implicit def ordering: Ordering[K] = toSortedMap.ordering
    private implicit def order: Order[K] = Order.fromOrdering

    /**
      * Alias for [[concat]]
      */
    def ++(as: NonEmptyMap[K, A]): NonEmptyMap[K, A] = concat(as)
    /**
      * Appends this NEM to another NEM, producing a new `NonEmptyMap`.
      */
    def concat(as: NonEmptyMap[K, A]): NonEmptyMap[K, A] = NonEmptyMap.create(toSortedMap ++ as.toSortedMap)

    /**
      * Removes a key from this map, returning a new SortedMap.
      */
    def -(key: K): SortedMap[K, A] = toSortedMap - key


    /**
      * Adds a key-value pair to this map, returning a new `NonEmptyMap`.
      * */
    def add(ka: (K, A)): NonEmptyMap[K, A] = NonEmptyMap.create(toSortedMap + ka)

    /**
      * Applies f to all the elements
      */
    def map[B](f: A ⇒ B): NonEmptyMap[K, B] =
      NonEmptyMap.create(Functor[SortedMap[K, ?]].map(toSortedMap)(f))

    /**
      * Optionally returns the value associated with the given key.
      * {{{
      * scala> import cats.data.NonEmptyMap
      * scala> import cats.implicits._
      * scala> val nem = NonEmptyMap.of("A" -> 1, "B" -> 2)
      * scala> nem.get("B")
      * res0: Option[Int] = Some(2)
      * }}}
      */
    def get(k: K): Option[A] = toSortedMap.get(k)

    /**
      * Returns a `SortedSet` containing all the keys of this map.
      * {{{
      * scala> import cats.data.NonEmptyMap
      * scala> import cats.implicits._
      * scala> val nem = NonEmptyMap.of(1 -> "A", 2 -> "B")
      * scala> nem.keys
      * res0: scala.collection.immutable.SortedSet[Int] = Set(1, 2)
      * }}}
      */
    def keys: SortedSet[K] = toSortedMap.keySet

    /**
      * Returns the first key-value pair of this map.
      */
    def head: (K, A) = toSortedMap.head
    /**
      * Returns the first key-value pair of this map.
      */
    def last: (K, A) = toSortedMap.last

    /**
      * Returns all the key-value pairs, except for the first.
      */
    def tail: SortedMap[K, A] = toSortedMap.tail

    /**
      * Alias for [[get]]
      *
      * {{{
      * scala> import cats.data.NonEmptyMap
      * scala> import cats.implicits._
      * scala> val nem = NonEmptyMap.of("A" -> 1, "B" -> 2)
      * scala> nem("A")
      * res0: Option[Int] = Some(1)
      * }}}
      */
    def apply(key: K): Option[A] = get(key)

    /**
      * Checks whether this map contains a binding for the given key.
      */
    def contains(key: K): Boolean = toSortedMap.contains(key)


    /**
      * Tests whether a predicate holds for all elements of this map.
      */
    def forall(p: A ⇒ Boolean): Boolean = toSortedMap.forall { case (_, a) => p(a) }

    /**
      * Tests whether a predicate holds for at least one element of this map.
      */
    def exists(f: A ⇒ Boolean): Boolean = toSortedMap.exists { case (_, a) => f(a) }

    /**
      * Returns the first value along with its key, that matches the given predicate.
      */
    def find(f: A ⇒ Boolean): Option[(K, A)] = toSortedMap.find { case (_, a) => f(a) }

    /**
      * Filters all elements of this map that do not satisfy the given predicate.
      */
    def filter(p: A ⇒ Boolean): SortedMap[K, A] = toSortedMap.filter  { case (_, a) => p(a) }

    /**
      * Filters all elements of this map that satisfy the given predicate.
      */
    def filterNot(p: A ⇒ Boolean): SortedMap[K, A] = filter(t => !p(t))


    /**
      * Left-associative fold using f.
      */
    def foldLeft[B](b: B)(f: (B, A) => B): B =
      toSortedMap.foldLeft(b)((b, t) => f(b, t._2))

    /**
      * Right-associative fold using f.
      */
    def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[SortedMap[K, ?]].foldRight(toSortedMap, lb)(f)

    /**
      * Left-associative reduce using f.
      */
    def reduceLeft(f: (A, A) => A): A =
      reduceLeftTo(identity)(f)


    /**
      * Apply `f` to the "initial element" of this map and lazily combine it
      * with every other value using the given function `g`.
      */
    def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B =
      tail.foldLeft(f(head._2))((b, a) => g(b, a._2))

    /**
      * Right-associative reduce using f.
      */
    def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] =
      reduceRightTo(identity)(f)

    /**
      * Apply `f` to the "initial element" of this map and lazily combine it
      * with every other value using the given function `g`.
      */
    def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      Always((head, tail)).flatMap { case ((_, a), ga) =>
        Foldable[SortedMap[K, ?]].reduceRightToOption(ga)(f)(g).flatMap {
          case Some(b) => g(a, Now(b))
          case None => Later(f(a))
        }
      }


    /**
      * Reduce using the Semigroup of A
      */
    def reduce(implicit S: Semigroup[A]): A =
      reduceLeft(S.combine)

    private def reduceRightToOptionWithKey[V, B](fa: SortedMap[K, V])(f: (K, V) => B)(g: ((K, V), Eval[B]) => Eval[B]): Eval[Option[B]] =
      Foldable.iterateRight(fa.toIterable, Now(Option.empty[B])) { (a, lb) =>
        lb.flatMap {
          case Some(b) => g(a, Now(b)).map(Some(_))
          case None => Later(Some(f.tupled(a)))
        }
      }

    /**
      * Given a function which returns a G effect, thread this effect
      * through the running of this function on all the values in this map,
      * returning an NonEmptyMap[K, B] in a G context.
      */
    def nonEmptyTraverse[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[NonEmptyMap[K, B]] =
      reduceRightToOptionWithKey[A, G[SortedMap[K, B]]](tail)({ case (k, a) =>
        G.map(f(a))(b => SortedMap.empty[K, B] + ((k, b)))
      }) { (t, lglb) =>
        G.map2Eval(f(t._2), lglb)((b, bs) => bs + ((t._1, b)))
      }.map {
        case None => G.map(f(head._2))(a => NonEmptyMap.one(head._1, a))
        case Some(gtail) => G.map2(f(head._2), gtail)((a, bs) => NonEmptyMap((head._1, a), bs))
      }.value

    /**
      * Typesafe stringification method.
      *
      * This method is similar to .toString except that it stringifies
      * values according to Show[_] instances, rather than using the
      * universal .toString method.
      */
    def show(implicit A: Show[A], K: Show[K]): String =
      s"NonEmpty${Show[SortedMap[K, A]].show(toSortedMap)}"

    override def toString: String = s"NonEmpty${toSortedMap.toString}"

    /**
      * Typesafe equality operator.
      *
      * This method is similar to == except that it only allows two
      * NonEmptySet[A] values to be compared to each other, and uses
      * equality provided by Eq[_] instances, rather than using the
      * universal equality provided by .equals.
      */
    def ===(that: NonEmptyMap[K, A])(implicit A: Eq[A]): Boolean =
      Eq[SortedMap[K, A]].eqv(toSortedMap, that.toSortedMap)

    /**
      * Returns the amount of key-value pars in this map.
      */
    def length: Int = toSortedMap.size
  }


}

private[data] sealed abstract class NonEmptyMapInstances {


  implicit def catsDataInstancesForNonEmptyMap[K: Order]: SemigroupK[NonEmptyMap[K, ?]] with NonEmptyTraverse[NonEmptyMap[K, ?]] =
    new SemigroupK[NonEmptyMap[K, ?]] with NonEmptyTraverse[NonEmptyMap[K, ?]] {

      def combineK[A](a: NonEmptyMap[K, A], b: NonEmptyMap[K, A]): NonEmptyMap[K, A] =
        a ++ b

      override def size[A](fa: NonEmptyMap[K, A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptyMap[K, A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyMap[K, A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyMap[K, A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyMap[K, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.reduceRightTo(f)(g)

      def nonEmptyTraverse[G[_], A, B](fa: NonEmptyMap[K, A])(f: A => G[B])(implicit G: Apply[G]) =
        fa nonEmptyTraverse f

      override def foldLeft[A, B](fa: NonEmptyMap[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyMap[K, A])(f: A => B)(implicit B: Monoid[B]): B =
        fa.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))

      override def fold[A](fa: NonEmptyMap[K, A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyMap[K, A])(f: A => Boolean): Option[A] =
        fa.find(f).map(_._2)

      override def forall[A](fa: NonEmptyMap[K, A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyMap[K, A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toNonEmptyList[A](fa: NonEmptyMap[K, A]): NonEmptyList[A] =
        NonEmptyList(fa.head._2, fa.tail.toList.map(_._2))
    }

  implicit def catsDataEqForNonEmptyMap[K: Order, A: Eq]: Eq[NonEmptyMap[K, A]] =
    new Eq[NonEmptyMap[K, A]]{
      def eqv(x: NonEmptyMap[K, A], y: NonEmptyMap[K, A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyMap[K: Show, A: Show]: Show[NonEmptyMap[K, A]] =
    Show.show[NonEmptyMap[K, A]](_.show)

  implicit def catsDataBandForNonEmptyMap[K, A]: Band[NonEmptyMap[K, A]] = new Band[NonEmptyMap[K, A]] {
    def combine(x: NonEmptyMap[K, A], y: NonEmptyMap[K, A]): NonEmptyMap[K, A] = x ++ y
  }
}

