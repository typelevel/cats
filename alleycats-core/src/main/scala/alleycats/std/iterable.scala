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

package alleycats
package std

import cats.data.Chain
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq
import cats.{Applicative, Eval, Foldable, Monoid, Traverse, TraverseFilter}

import scala.collection.immutable.{IndexedSeq => ImIndexedSeq}

object iterable extends IterableInstances

trait IterableInstances {
  implicit def alleycatsStdIterableTraverse: Traverse[Iterable] =
    new Traverse[Iterable] {
      override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Iterable[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def reduceLeftOption[A](fa: Iterable[A])(f: (A, A) => A): Option[A] = fa.reduceLeftOption(f)

      override def collectFirst[A, B](fa: Iterable[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def fold[A](fa: Iterable[A])(implicit A: Monoid[A]): A = fa.fold(A.empty)(A.combine)

      override def find[A](fa: Iterable[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def toIterable[A](fa: Iterable[A]): Iterable[A] = fa

      override def exists[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.exists(p)

      override def forall[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.forall(p)

      override def toList[A](fa: Iterable[A]): List[A] = fa.toList

      override def isEmpty[A](fa: Iterable[A]): Boolean = fa.isEmpty

      override def nonEmpty[A](fa: Iterable[A]): Boolean = fa.nonEmpty

      // Adapted from List and Vector instances.
      override def traverse[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] =
        if (fa.isEmpty) G.pure(Iterable.empty)
        else G.map(Chain.traverseViaChain(toImIndexedSeq(fa))(f))(_.toVector)
    }

  implicit def alleycatsStdIterableTraverseFilter: TraverseFilter[Iterable] = new TraverseFilter[Iterable] {
    override def traverse: Traverse[Iterable] = alleycatsStdIterableTraverse

    override def traverseFilter[G[_], A, B](
      fa: Iterable[A]
    )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Iterable[B]] =
      if (fa.isEmpty) G.pure(Iterable.empty)
      else G.map(Chain.traverseFilterViaChain(toImIndexedSeq(fa))(f))(_.toVector)
  }

  private def toImIndexedSeq[A](fa: Iterable[A]): ImIndexedSeq[A] = fa match {
    case iseq: ImIndexedSeq[A] => iseq
    case _ =>
      val as = collection.mutable.ArrayBuffer[A]()
      as ++= fa
      wrapMutableIndexedSeq(as)
  }

  @deprecated("use alleycatsStdIterableTraverse", "2.7.1")
  val alleycatsStdIterableFoldable: Foldable[Iterable] = alleycatsStdIterableTraverse
}
