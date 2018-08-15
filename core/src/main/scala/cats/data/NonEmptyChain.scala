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

import NonEmptyChainImpl.{create, unwrap}
import cats.Order
import cats.kernel._

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable.ListBuffer


private[data] object NonEmptyChainImpl extends NonEmptyChainInstances {

  private[data] type Base
  private[data] trait Tag extends Any
  type Type[+A] <: Base with Tag

  private[cats] def create[A](s: Chain[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[cats] def unwrap[A](s: Type[A]): Chain[A] =
    s.asInstanceOf[Chain[A]]

  def fromChain[A](as: Chain[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(create(as)) else None

  def fromChainUnsafe[A](chain: Chain[A]): NonEmptyChain[A] =
    if (chain.nonEmpty) create(chain)
    else throw new IllegalArgumentException("Cannot create NonEmptyChain from empty chain")

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyChain[A] =
    create(Chain.fromSeq(as.toList))

  def fromNonEmptyVector[A](as: NonEmptyVector[A]): NonEmptyChain[A] =
    create(Chain.fromSeq(as.toVector))

  def fromSeq[A](as: Seq[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(create(Chain.fromSeq(as))) else None

  def fromChainPrepend[A](a: A, ca: Chain[A]): NonEmptyChain[A] =
    create(a +: ca)

  def fromChainAppend[A](ca: Chain[A], a: A): NonEmptyChain[A] =
    create(ca :+ a)

  def apply[A](a: A, as: A*): NonEmptyChain[A] =
    create(Chain.concat(Chain.one(a), Chain.fromSeq(as)))

  def one[A](a: A): NonEmptyChain[A] = create(Chain.one(a))

  implicit def catsNonEmptyChainOps[A](value: NonEmptyChain[A]): NonEmptyChainOps[A] =
    new NonEmptyChainOps(value)
}


class NonEmptyChainOps[A](val value: NonEmptyChain[A]) extends AnyVal {

  /**
   * Converts this chain to a `Chain`
   */
  final def toChain: Chain[A] = NonEmptyChainImpl.unwrap(value)

  /**
   * Returns a new NonEmptyChain consisting of `a` followed by this. O(1) runtime.
   */
  final def prepend[A2 >: A](a: A2): NonEmptyChain[A2] =
    create(toChain.prepend(a))

  /**
   * Alias for [[prepend]].
   */
  final def +:[A2 >: A](a: A2): NonEmptyChain[A2] =
    prepend(a)

  /**
   * Returns a new Chain consisting of this followed by `a`. O(1) runtime.
   */
  final def append[A2 >: A](a: A2): NonEmptyChain[A2] =
    create(toChain.append(a))

  /**
   * Alias for [[append]].
   */
  final def :+[A2 >: A](a: A2): NonEmptyChain[A2] =
    append(a)

  /**
   * Concatenates this with `c` in O(1) runtime.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 4, 5)
   * scala> nec ++ NonEmptyChain(7, 8)
   * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 4, 5, 7, 8)
   * }}}
   */
  final def concat[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
    create(toChain ++ c.toChain)


  /**
   * Alias for concat
   */
  final def ++[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
    concat(c)

  /**
   * Appends the given chain in O(1) runtime.
   */
  final def appendChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    if (c.isEmpty) value
    else create(toChain ++ c)

  /**
   * Alias for `appendChain`
   */
  final def :++[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    appendChain(c)

  /**
   * Prepends the given chain in O(1) runtime.
   */
  final def prependChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    if (c.isEmpty) value
    else create(c ++ toChain)

  /**
   * Alias for `prependChain`
   */
  final def ++:[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    prependChain(c)

  /**
   * Applies f to all the elements
   */
  final def map[B](f: A => B): NonEmptyChain[B] =
    create(toChain.map(f))


  /**
   * Yields to Some(a, Chain[A]) with `a` removed where `f` holds for the first time,
   * otherwise yields None, if `a` was not found
   * Traverses only until `a` is found.
   */
  final def deleteFirst(f: A => Boolean): Option[(A, Chain[A])] =
    toChain.deleteFirst(f)


  /**
   * Converts this chain to a `NonEmptyList`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
   * scala> nec.toNonEmptyList
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
   * }}}
   */
  final def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromListUnsafe(toChain.toList)

  /**
   * Converts this chain to a `NonEmptyVector`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
   * scala> nec.toNonEmptyVector
   * res0: cats.data.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3, 4, 5)
   * }}}
   */
  final def toNonEmptyVector: NonEmptyVector[A] = NonEmptyVector.fromVectorUnsafe(toChain.toVector)

  /**
   * Returns the head and tail of this NonEmptyChain. Amortized O(1).
   */
  final def uncons: (A, Chain[A]) = toChain.uncons.get

  /**
   * Returns the first element of this chain.
   */
  final def head: A = uncons._1

  /**
   * Returns all but the first element of this chain.
   */
  final def tail: Chain[A] = uncons._2

  /**
   * Tests if some element is contained in this chain.
   */
  final def contains(a: A)(implicit A: Eq[A]): Boolean = toChain.contains(a)

  /**
   * Tests whether a predicate holds for all elements of this chain.
   */
  final def forall(p: A ⇒ Boolean): Boolean = toChain.forall(p)

  /**
   * Tests whether a predicate holds for at least one element of this chain.
   */
  final def exists(f: A ⇒ Boolean): Boolean = toChain.exists(f)

  /**
   * Returns the first value that matches the given predicate.
   */
  final def find(f: A ⇒ Boolean): Option[A] = toChain.find(f)

  /**
   * Returns a new `Chain` containing all elements where the result of `pf` is final defined.
   */
  final def collect[B](pf: PartialFunction[A, B]): Chain[B] = toChain.collect(pf)

  /**
   * Filters all elements of this chain that do not satisfy the given predicate.
   */
  final def filter(p: A ⇒ Boolean): Chain[A] = toChain.filter(p)

  /**
   * Filters all elements of this chain that satisfy the given predicate.
   */
  final def filterNot(p: A ⇒ Boolean): Chain[A] = filter(t => !p(t))


  /**
   * Left-associative fold using f.
   */
  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    toChain.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    toChain.foldRight(z)(f)

  /**
   * Left-associative reduce using f.
   */
  final def reduceLeft(f: (A, A) => A): A = {
    val iter = toChain.iterator
    var result = iter.next
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /**
   * Apply `f` to the "initial element" of this chain and lazily combine it
   * with every other value using the given function `g`.
   */
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    val iter = toChain.iterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(result, iter.next) }
    result
  }

  /**
   * Right-associative reduce using f.
   */
  final def reduceRight(f: (A, A) => A): A = {
    val iter = toChain.reverseIterator
    var result = iter.next
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /**
   * Apply `f` to the "initial element" of this chain and lazily combine it
   * with every other value using the given function `g`.
   */
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
    val iter = toChain.reverseIterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(iter.next, result) }
    result
  }

  /**
   * Reduce using the Semigroup of A
   */
  final def reduce[AA >: A](implicit S: Semigroup[AA]): AA = {
    S.combineAllOption(iterator).get
  }

  /**
   * Applies the supplied function to each element and returns a new NonEmptyChain from the concatenated results
   */
  final def flatMap[B](f: A => NonEmptyChain[B]): NonEmptyChain[B] = {
    create(toChain.flatMap(f andThen (_.toChain)))
  }


  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  final def show(implicit A: Show[A]): String =
    s"NonEmpty${Show[Chain[A]].show(toChain)}"

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptyChain[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  final def ===(that: NonEmptyChain[A])(implicit A: Eq[A]): Boolean =
    Eq[Chain[A]].eqv(toChain, that.toChain)

  /**
   * Returns the number of elements in this chain.
   */
  final def length: Long = toChain.size

  /** Alias for length */
  final def size: Long = length

  /**
   * Zips this `NonEmptyChain` with another `NonEmptyChain` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val as = NonEmptyChain(1, 2, 3)
   * scala> val bs = NonEmptyChain("A", "B", "C")
   * scala> as.zipWith(bs)(_ + _)
   * res0: cats.data.NonEmptyChain[String] = Chain(1A, 2B, 3C)
   * }}}
   */
  final def zipWith[B, C](b: NonEmptyChain[B])(f: (A, B) => C): NonEmptyChain[C] =
    create(toChain.zipWith(b.toChain)(f))

  /**
   * Groups elements inside this `NonEmptyChain` according to the `Order`
   * of the keys produced by the given mapping function.
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyChain[A]] =
    toChain.groupBy(f).asInstanceOf[NonEmptyMap[B, NonEmptyChain[A]]]

  final def iterator: Iterator[A] = toChain.iterator

  final def reverseIterator: Iterator[A] = toChain.reverseIterator

  /** Reverses this `NonEmptyChain` */
  final def reverse: NonEmptyChain[A] =
    create(toChain.reverse)
}

private[data] sealed abstract class NonEmptyChainInstances {
  implicit val catsDataInstancesForNonEmptyChain: SemigroupK[NonEmptyChain]
    with NonEmptyTraverse[NonEmptyChain]
    with Bimonad[NonEmptyChain] =
    new SemigroupK[NonEmptyChain] with NonEmptyTraverse[NonEmptyChain] with Bimonad[NonEmptyChain] {

      def combineK[A](a: NonEmptyChain[A], b: NonEmptyChain[A]): NonEmptyChain[A] =
        a ++ b

      def pure[A](x: A): NonEmptyChain[A] = NonEmptyChain.one(x)

      def flatMap[A, B](fa: NonEmptyChain[A])(f: A => NonEmptyChain[B]): NonEmptyChain[B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => NonEmptyChain[Either[A, B]]): NonEmptyChain[B] =
        create(Monad[Chain].tailRecM(a)(a => unwrap(f(a))))

      def extract[A](x: NonEmptyChain[A]): A = x.head

      def coflatMap[A, B](fa: NonEmptyChain[A])(f: NonEmptyChain[A] => B): NonEmptyChain[B] = {
        @tailrec def go(as: Chain[A], res: ListBuffer[B]): Chain[B] =
          as.uncons match {
            case Some((h, t)) => go(t, res += f(NonEmptyChain.fromChainPrepend(h, t)))
            case None => Chain.fromSeq(res.result())
          }
        NonEmptyChain.fromChainPrepend(f(fa), go(fa.tail, ListBuffer.empty))
      }

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: NonEmptyChain[A])(f: A => G[B]): G[NonEmptyChain[B]] =
        Foldable[Chain].reduceRightToOption[A, G[Chain[B]]](fa.tail)(a => Apply[G].map(f(a))(Chain.one)) { (a, lglb) =>
          Apply[G].map2Eval(f(a), lglb)(_ +: _)
        }.map {
          case None => Apply[G].map(f(fa.head))(NonEmptyChain.one)
          case Some(gtail) => Apply[G].map2(f(fa.head), gtail)((h, t) => create(Chain.one(h) ++ t))
        }.value

      override def size[A](fa: NonEmptyChain[A]): Long = fa.length

      override def reduceLeft[A](fa: NonEmptyChain[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyChain[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

      override def foldLeft[A, B](fa: NonEmptyChain[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyChain[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyChain[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toChain.iterator.map(f))

      override def fold[A](fa: NonEmptyChain[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyChain[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptyChain[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyChain[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptyChain[A]): List[A] = fa.toChain.toList

      override def toNonEmptyList[A](fa: NonEmptyChain[A]): NonEmptyList[A] =
        fa.toNonEmptyList
    }

  implicit def catsDataEqForNonEmptyChain[A: Eq]: Eq[NonEmptyChain[A]] =
    new Eq[NonEmptyChain[A]]{
      def eqv(x: NonEmptyChain[A], y: NonEmptyChain[A]): Boolean = x.toChain === y.toChain
    }

  implicit def catsDataShowForNonEmptyChain[A](implicit A: Show[A]): Show[NonEmptyChain[A]] =
    Show.show[NonEmptyChain[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyChain[A]: Semigroup[NonEmptyChain[A]] = new Semigroup[NonEmptyChain[A]] {
    def combine(x: NonEmptyChain[A], y: NonEmptyChain[A]): NonEmptyChain[A] = x ++ y
  }
}

