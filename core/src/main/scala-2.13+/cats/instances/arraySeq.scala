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
package instances

import cats.data.{Chain, Ior}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder

trait ArraySeqInstances extends cats.kernel.instances.ArraySeqInstances {
  implicit def catsStdInstancesForArraySeq
    : Traverse[ArraySeq] & Monad[ArraySeq] & Alternative[ArraySeq] & CoflatMap[ArraySeq] & Align[ArraySeq] =
    ArraySeqInstances.stdInstances

  implicit def catsStdTraverseFilterForArraySeq: TraverseFilter[ArraySeq] =
    ArraySeqInstances.stdTraverseFilterInstance

  implicit def catsStdShowForArraySeq[A](implicit ev: Show[A]): Show[ArraySeq[A]] =
    _.iterator.map(ev.show).mkString("ArraySeq(", ", ", ")")
}

private[cats] object ArraySeqInstances {
  final private val stdInstances
    : Traverse[ArraySeq] & Monad[ArraySeq] & Alternative[ArraySeq] & CoflatMap[ArraySeq] & Align[ArraySeq] =
    new Traverse[ArraySeq]
      with Monad[ArraySeq]
      with Alternative[ArraySeq]
      with CoflatMap[ArraySeq]
      with Align[ArraySeq] {
      def empty[A]: ArraySeq[A] =
        ArraySeq.untagged.empty

      def combineK[A](xs: ArraySeq[A], ys: ArraySeq[A]): ArraySeq[A] =
        xs.concat(ys)

      override def fromIterableOnce[A](as: IterableOnce[A]): ArraySeq[A] = ArraySeq.untagged.from(as)

      override def prependK[A](a: A, fa: ArraySeq[A]): ArraySeq[A] = fa.prepended(a)

      override def appendK[A](fa: ArraySeq[A], a: A): ArraySeq[A] = fa.appended(a)

      override def algebra[A]: Monoid[ArraySeq[A]] =
        new cats.kernel.instances.ArraySeqInstances.ArraySeqMonoid

      def pure[A](a: A): ArraySeq[A] =
        ArraySeq.untagged.fill(n = 1)(elem = a)

      override def map[A, B](fa: ArraySeq[A])(f: A => B): ArraySeq[B] =
        fa.map(f)

      def flatMap[A, B](fa: ArraySeq[A])(f: A => ArraySeq[B]): ArraySeq[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: ArraySeq[A])(f: ArraySeq[A] => B): ArraySeq[B] = {
        @tailrec def loop(builder: Builder[B, ArraySeq[B]], as: ArraySeq[A]): ArraySeq[B] =
          as match {
            case _ +: rest => loop(builder += f(as), rest)
            case _         => builder.result()
          }
        loop(ArraySeq.untagged.newBuilder[B], fa)
      }

      override def map2[A, B, Z](fa: ArraySeq[A], fb: ArraySeq[B])(f: (A, B) => Z): ArraySeq[Z] =
        if (fb.isEmpty) ArraySeq.untagged.empty // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: ArraySeq[A], fb: Eval[ArraySeq[B]])(f: (A, B) => Z): Eval[ArraySeq[Z]] =
        if (fa.isEmpty) Eval.now(ArraySeq.untagged.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def foldLeft[A, B](fa: ArraySeq[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: ArraySeq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(i: Int): Eval[B] =
          if (i < fa.length) f(fa(i), Eval.defer(loop(i + 1))) else lb

        Eval.defer(loop(0))
      }

      override def foldMap[A, B](fa: ArraySeq[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_], A, B](fa: ArraySeq[A])(f: A => G[B])(implicit G: Applicative[G]): G[ArraySeq[B]] =
        G match {
          case x: StackSafeMonad[G] =>
            x.map(Traverse.traverseDirectly(fa.iterator)(f)(x))(_.iterator.to(ArraySeq.untagged))
          case _ =>
            G.map(Chain.traverseViaChain(fa)(f))(_.iterator.to(ArraySeq.untagged))

        }

      override def traverseVoid[G[_], A, B](fa: ArraySeq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
        G match {
          case x: StackSafeMonad[G] => Traverse.traverseVoidDirectly(fa)(f)(x)
          case _ =>
            foldRight(fa, Eval.now(G.unit)) { (a, acc) =>
              G.map2Eval(f(a), acc) { (_, _) =>
                ()
              }
            }.value
        }

      override def mapAccumulate[S, A, B](init: S, fa: ArraySeq[A])(f: (S, A) => (S, B)): (S, ArraySeq[B]) =
        StaticMethods.mapAccumulateFromStrictFunctor(init, fa, f)(this)

      override def mapWithIndex[A, B](fa: ArraySeq[A])(f: (A, Int) => B): ArraySeq[B] =
        ArraySeq.untagged.tabulate(n = fa.length) { i =>
          f(fa(i), i)
        }

      override def zipWithIndex[A](fa: ArraySeq[A]): ArraySeq[(A, Int)] =
        fa.zipWithIndex

      def tailRecM[A, B](a: A)(fn: A => ArraySeq[Either[A, B]]): ArraySeq[B] = {
        val buf = ArraySeq.untagged.newBuilder[B]

        @tailrec
        def loop(state: List[Iterator[Either[A, B]]]): Unit =
          state match {
            case h :: tail if h.isEmpty =>
              loop(state = tail)
            case h :: tail =>
              h.next() match {
                case Right(b) =>
                  buf += b
                  loop(state)
                case Left(a) =>
                  loop(state = fn(a).iterator :: h :: tail)
              }

            case Nil => ()
          }

        loop(state = fn(a).iterator :: Nil)

        buf.result()
      }

      override def exists[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def get[A](fa: ArraySeq[A])(idx: Long): Option[A] =
        if (idx >= 0 && idx < fa.length && idx.isValidInt) Some(fa(idx.toInt)) else None

      override def isEmpty[A](fa: ArraySeq[A]): Boolean =
        fa.isEmpty

      override def foldM[G[_], A, B](fa: ArraySeq[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
        G.tailRecM((z, 0)) { case (b, i) =>
          if (i < fa.length) G.map(f(b, fa(i)))(b => Left((b, i + 1)))
          else G.pure(Right(b))
        }

      override def fold[A](fa: ArraySeq[A])(implicit A: Monoid[A]): A =
        A.combineAll(fa)

      override def toList[A](fa: ArraySeq[A]): List[A] =
        fa.toList

      override def toIterable[A](fa: ArraySeq[A]): Iterable[A] =
        fa

      override def reduceLeftOption[A](fa: ArraySeq[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: ArraySeq[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def collectFirst[A, B](fa: ArraySeq[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: ArraySeq[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

      def functor: Functor[ArraySeq] = this

      def align[A, B](fa: ArraySeq[A], fb: ArraySeq[B]): ArraySeq[Ior[A, B]] = {
        val aLarger = fa.size >= fb.size
        if (aLarger) {
          fa.lazyZip(fb).map(Ior.both) ++ fa.drop(fb.size).map(Ior.left)
        } else {
          fa.lazyZip(fb).map(Ior.both) ++ fb.drop(fa.size).map(Ior.right)
        }
      }

    }

  final private val stdTraverseFilterInstance: TraverseFilter[ArraySeq] =
    new TraverseFilter[ArraySeq] {
      val traverse: Traverse[ArraySeq] = stdInstances

      override def mapFilter[A, B](fa: ArraySeq[A])(f: (A) => Option[B]): ArraySeq[B] =
        fa.collect(Function.unlift(f))

      override def filter[A](fa: ArraySeq[A])(f: (A) => Boolean): ArraySeq[A] =
        fa.filter(f)

      override def filterNot[A](fa: ArraySeq[A])(f: (A) => Boolean): ArraySeq[A] =
        fa.filterNot(f)

      override def collect[A, B](fa: ArraySeq[A])(f: PartialFunction[A, B]): ArraySeq[B] =
        fa.collect(f)

      override def flattenOption[A](fa: ArraySeq[Option[A]]): ArraySeq[A] =
        fa.flatten

      def traverseFilter[G[_], A, B](
        fa: ArraySeq[A]
      )(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[ArraySeq[B]] =
        G match {
          case x: StackSafeMonad[G] =>
            x.map(TraverseFilter.traverseFilterDirectly(fa.iterator)(f)(x))(
              _.iterator.to(ArraySeq.untagged)
            )
          case _ =>
            fa.foldRight(Eval.now(G.pure(ArraySeq.untagged.empty[B]))) { case (x, xse) =>
              G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o))
            }.value

        }

      override def filterA[G[_], A](fa: ArraySeq[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[ArraySeq[A]] =
        fa.foldRight(Eval.now(G.pure(ArraySeq.untagged.empty[A]))) { case (x, xse) =>
          G.map2Eval(f(x), xse)((b, vec) => if (b) x +: vec else vec)
        }.value
    }
}
