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

package alleycats.std

import cats.data.Chain
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq
import cats.{Applicative, Eval, Foldable, Monoid, Traverse, TraverseFilter}

import scala.collection.immutable.{IndexedSeq => ImIndexedSeq}

object seq extends SeqInstances

trait SeqInstances {
  implicit def alleycatsStdSeqTraverse: Traverse[Seq] =
    new Traverse[Seq] {
      override def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Seq[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def reduceLeftOption[A](fa: Seq[A])(f: (A, A) => A): Option[A] = fa.reduceLeftOption(f)

      override def collectFirst[A, B](fa: Seq[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def fold[A](fa: Seq[A])(implicit A: Monoid[A]): A = fa.fold(A.empty)(A.combine)

      override def find[A](fa: Seq[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def toIterable[A](fa: Seq[A]): Iterable[A] = fa

      override def exists[A](fa: Seq[A])(p: A => Boolean): Boolean = fa.exists(p)

      override def forall[A](fa: Seq[A])(p: A => Boolean): Boolean = fa.forall(p)

      override def toList[A](fa: Seq[A]): List[A] = fa.toList

      override def isEmpty[A](fa: Seq[A]): Boolean = fa.isEmpty

      override def nonEmpty[A](fa: Seq[A]): Boolean = fa.nonEmpty

      // Adapted from List and Vector instances.
      override def traverse[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Seq[B]] =
        if (fa.isEmpty) G.pure(Seq.empty)
        else G.map(Chain.traverseViaChain(toImIndexedSeq(fa))(f))(_.toVector)

      override def mapAccumulate[S, A, B](init: S, fa: Seq[A])(f: (S, A) => (S, B)): (S, Seq[B]) = {
        val iter = fa.iterator
        var s = init
        val vec = Vector.newBuilder[B]
        while (iter.hasNext) {
          val (snext, b) = f(s, iter.next())
          vec += b
          s = snext
        }
        (s, vec.result())
      }

      override def zipWithIndex[A](fa: Seq[A]): Seq[(A, Int)] =
        fa.zipWithIndex

      override def mapWithIndex[A, B](fa: Seq[A])(f: (A, Int) => B): Seq[B] =
        fa.zipWithIndex.map { case (a, i) => f(a, i) }
    }

  implicit def alleycatsStdSeqTraverseFilter: TraverseFilter[Seq] = new TraverseFilter[Seq] {
    override def traverse: Traverse[Seq] = alleycatsStdSeqTraverse

    override def traverseFilter[G[_], A, B](
      fa: Seq[A]
    )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Seq[B]] =
      if (fa.isEmpty) G.pure(Seq.empty)
      else G.map(Chain.traverseFilterViaChain(toImIndexedSeq(fa))(f))(_.toVector)
  }

  private def toImIndexedSeq[A](fa: Seq[A]): ImIndexedSeq[A] = fa match {
    case iseq: ImIndexedSeq[A] => iseq
    case _ =>
      val as = collection.mutable.ArrayBuffer[A]()
      as ++= fa
      wrapMutableIndexedSeq(as)
  }
}
