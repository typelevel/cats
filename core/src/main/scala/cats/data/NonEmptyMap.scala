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

final class NonEmptyMap[K, A] private(val value: SortedMap[K, A]) extends AnyVal {

  private implicit def ordering: Ordering[K] = value.ordering
  private implicit def order: Order[K] = Order.fromOrdering

  def ++(as: NonEmptyMap[K, A]): NonEmptyMap[K, A] = concat(as)
  def concat(as: NonEmptyMap[K, A]): NonEmptyMap[K, A] = new NonEmptyMap(value ++ as.value)
  def -(key: K): SortedMap[K, A] = value - key
  def +(ka: (K, A)): NonEmptyMap[K, A] = new NonEmptyMap(value + ka)

  def map[B](f: A ⇒ B): NonEmptyMap[K, B] =
    new NonEmptyMap(Functor[SortedMap[K, ?]].map(value)(f))

  def get(k: K): Option[A] = value.get(k)

  def keys: SortedSet[K] = value.keySet

  def toNonEmptyList: NonEmptyList[(K, A)] = NonEmptyList.fromListUnsafe(value.toList)

  def head: (K, A) = value.head
  def last: (K, A) = value.last
  def tail: SortedMap[K, A] = value.tail

  def apply(key: K): Option[A] = get(key)
  def contains(key: K): Boolean = value.contains(key)

  def size: Int = value.size
  def forall(p: A ⇒ Boolean): Boolean = value.forall { case (_, a) => p(a) }
  def exists(f: A ⇒ Boolean): Boolean = value.exists { case (_, a) => f(a) }
  def find(f: A ⇒ Boolean): Option[(K, A)] = value.find { case (_, a) => f(a) }
  def filter(p: A ⇒ Boolean): SortedMap[K, A] = value.filter  { case (_, a) => p(a) }
  def filterNot(p: A ⇒ Boolean): SortedMap[K, A] = filter(t => !p(t))


  /**
    * Left-associative fold using f.
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    value.foldLeft(b)((b, t) => f(b, t._2))

  /**
    * Right-associative fold using f.
    */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[SortedMap[K, ?]].foldRight(value, lb)(f)

  /**
    * Left-associative reduce using f.
    */
  def reduceLeft(f: (A, A) => A): A =
    reduceLeftTo(identity)(f)

  def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B =
    tail.foldLeft(f(head._2))((b, a) => g(b, a._2))

  /**
    * Right-associative reduce using f.
    */
  def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(identity)(f)

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

  def nonEmptyTraverse[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[NonEmptyMap[K, B]] =
    reduceRightToOptionWithKey[A, G[SortedMap[K, B]]](tail)({ case (k, a) =>
      G.map(f(a))(b => SortedMap.empty[K, B] + ((k, b)))
    }) { (t, lglb) =>
      G.map2Eval(f(t._2), lglb)((b, bs) => bs + ((t._1, b)))
    }.map {
      case None => G.map(f(head._2))(a => NonEmptyMap.one(head._1, a))
      case Some(gtail) => G.map2(f(head._2), gtail)((a, bs) => NonEmptyMap((head._1, a), bs))
    }.value

  def toMap: SortedMap[K, A] = value

  /**
    * Typesafe stringification method.
    *
    * This method is similar to .toString except that it stringifies
    * values according to Show[_] instances, rather than using the
    * universal .toString method.
    */
  def show(implicit A: Show[A], K: Show[K]): String =
    s"NonEmpty${Show[SortedMap[K, A]].show(value)}"

  /**
    * Typesafe equality operator.
    *
    * This method is similar to == except that it only allows two
    * NonEmptySet[A] values to be compared to each other, and uses
    * equality provided by Eq[_] instances, rather than using the
    * universal equality provided by .equals.
    */
  def ===(that: NonEmptyMap[K, A])(implicit A: Eq[A]): Boolean =
    Eq[SortedMap[K, A]].eqv(value, that.toMap)

  def length: Int = size

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

object NonEmptyMap extends NonEmptyMapInstances {
  def fromMap[K: Order, A](as: SortedMap[K, A]): Option[NonEmptyMap[K, A]] =
    if (as.nonEmpty) Option(new NonEmptyMap(as)) else None

  def fromMapUnsafe[K: Order, A](m: SortedMap[K, A]): NonEmptyMap[K, A] =
    if (m.nonEmpty) new NonEmptyMap(m)
    else throw new IllegalArgumentException("Cannot create NonEmptyMap from empty map")

  def apply[K: Order, A](head: (K, A), tail: SortedMap[K, A]): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap(head)(Order[K].toOrdering) ++ tail)


  def of[K: Order, A](a: (K, A), as: (K, A)*): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap(as: _*)(Order[K].toOrdering) + a)

  def one[K: Order, A](k: K, a: A): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap((k, a))(Order[K].toOrdering))
}
