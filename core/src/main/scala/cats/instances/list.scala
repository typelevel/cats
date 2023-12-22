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

import cats.data.{Chain, Ior, ZipList}
import cats.instances.StaticMethods.appendAll
import cats.kernel.compat.scalaVersionSpecific._
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances extends cats.kernel.instances.ListInstances {

  implicit val catsStdInstancesForList
    : Traverse[List] with Alternative[List] with Monad[List] with CoflatMap[List] with Align[List] =
    new Traverse[List] with Alternative[List] with Monad[List] with CoflatMap[List] with Align[List] {
      def empty[A]: List[A] = Nil

      def combineK[A](x: List[A], y: List[A]): List[A] = x ::: y

      override def combineAllOptionK[A](as: IterableOnce[List[A]]): Option[List[A]] = {
        val iter = as.iterator
        if (iter.isEmpty) None else Some(appendAll(iter, List.newBuilder[A]).result())
      }

      override def fromIterableOnce[A](as: IterableOnce[A]): List[A] = as.iterator.toList

      override def prependK[A](a: A, fa: List[A]): List[A] = a :: fa

      override def appendK[A](fa: List[A], a: A): List[A] = fa :+ a

      def pure[A](x: A): List[A] = x :: Nil

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        if (fb.isEmpty) Nil // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      private[this] val evalNil: Eval[List[Nothing]] = Eval.now(Nil)

      override def map2Eval[A, B, Z](fa: List[A], fb: Eval[List[B]])(f: (A, B) => Z): Eval[List[Z]] =
        if (fa.isEmpty) evalNil // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
        val buf = List.newBuilder[B]
        @tailrec def go(lists: List[List[Either[A, B]]]): Unit =
          lists match {
            case (ab :: abs) :: tail =>
              ab match {
                case Right(b) => buf += b; go(abs :: tail)
                case Left(a)  => go(f(a) :: abs :: tail)
              }
            case Nil :: tail => go(tail)
            case Nil         => ()
          }
        go(f(a) :: Nil)
        buf.result()
      }

      def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = {
        @tailrec def loop(buf: ListBuffer[B], as: List[A]): List[B] =
          as match {
            case Nil       => buf.toList
            case _ :: rest => loop(buf += f(as), rest)
          }
        loop(ListBuffer.empty[B], fa)
      }

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: List[A]): Eval[B] =
          as match {
            case Nil    => lb
            case h :: t => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      override def foldMap[A, B](fa: List[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def foldMapK[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] = {
        def loop(fa: List[A]): Eval[G[B]] =
          fa match {
            case head :: tl => G.combineKEval(f(head), Eval.defer(loop(tl)))
            case Nil        => Eval.now(G.empty)
          }
        loop(fa).value
      }

      def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        if (fa.isEmpty) G.pure(Nil)
        else
          G match {
            case _ =>
              G.map(Chain.traverseViaChain {
                val as = collection.mutable.ArrayBuffer[A]()
                as ++= fa
                wrapMutableIndexedSeq(as)
              }(f))(_.toList)
          }

      /**
       * This avoids making a very deep stack by building a tree instead
       */
      override def traverse_[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = {
        G match {
          case _ =>
            // the cost of this is O(size log size)
            // c(n) = n + 2 * c(n/2) = n + 2(n/2 log (n/2)) = n + n (logn - 1) = n log n
            // invariant: size >= 1
            def runHalf(size: Int, fa: List[A]): Eval[G[Unit]] =
              if (size > 1) {
                val leftSize = size / 2
                val rightSize = size - leftSize
                val (leftL, rightL) = fa.splitAt(leftSize)
                runHalf(leftSize, leftL)
                  .flatMap { left =>
                    val right = runHalf(rightSize, rightL)
                    G.map2Eval(left, right) { (_, _) => () }
                  }
              } else {
                // avoid pattern matching when we know that there is only one element
                val a = fa.head
                // we evaluate this at most one time,
                // always is a bit cheaper in such cases
                //
                // Here is the point of the laziness using Eval:
                // we avoid calling f(a) or G.void in the
                // event that the computation has already
                // failed. We do not use laziness to avoid
                // traversing fa, which we will do fully
                // in all cases.
                Eval.always {
                  val gb = f(a)
                  G.void(gb)
                }
              }

            val len = fa.length
            if (len == 0) G.unit
            else runHalf(len, fa).value
        }
      }

      def functor: Functor[List] = this

      def align[A, B](fa: List[A], fb: List[B]): List[A Ior B] =
        alignWith(fa, fb)(identity)

      override def alignWith[A, B, C](fa: List[A], fb: List[B])(f: Ior[A, B] => C): List[C] = {
        @tailrec def loop(buf: ListBuffer[C], as: List[A], bs: List[B]): List[C] =
          (as, bs) match {
            case (a :: atail, b :: btail) => loop(buf += f(Ior.Both(a, b)), atail, btail)
            case (Nil, Nil)               => buf.toList
            case (arest, Nil)             => (buf ++= arest.map(a => f(Ior.left(a)))).toList
            case (Nil, brest)             => (buf ++= brest.map(b => f(Ior.right(b)))).toList
          }
        loop(ListBuffer.empty[C], fa, fb)
      }

      override def mapAccumulate[S, A, B](init: S, fa: List[A])(f: (S, A) => (S, B)): (S, List[B]) =
        StaticMethods.mapAccumulateFromStrictFunctor(init, fa, f)(this)

      override def mapWithLongIndex[A, B](fa: List[A])(f: (A, Long) => B): List[B] =
        StaticMethods.mapWithLongIndexFromStrictFunctor(fa, f)(this)

      override def mapWithIndex[A, B](fa: List[A])(f: (A, Int) => B): List[B] =
        StaticMethods.mapWithIndexFromStrictFunctor(fa, f)(this)

      override def zipWithIndex[A](fa: List[A]): List[(A, Int)] =
        fa.zipWithIndex

      override def partitionEither[A, B, C](
        fa: List[A]
      )(f: A => Either[B, C])(implicit A: Alternative[List]): (List[B], List[C]) =
        fa.foldRight((List.empty[B], List.empty[C]))((a, acc) =>
          f(a) match {
            case Left(b)  => (b :: acc._1, acc._2)
            case Right(c) => (acc._1, c :: acc._2)
          }
        )

      @tailrec
      override def get[A](fa: List[A])(idx: Long): Option[A] =
        fa match {
          case Nil => None
          case h :: tail =>
            if (idx < 0) None
            else if (idx == 0) Some(h)
            else get(tail)(idx - 1)
        }

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: List[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        def step(in: (List[A], B)): G[Either[(List[A], B), B]] =
          in match {
            case (Nil, b) => G.pure(Right(b))
            case (a :: tail, b) =>
              G.map(f(b, a)) { bnext =>
                Left((tail, bnext))
              }
          }

        G.tailRecM((fa, z))(step)
      }

      override def fold[A](fa: List[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: List[A]): List[A] = fa

      override def toIterable[A](fa: List[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: List[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: List[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def filter_[A](fa: List[A])(p: A => Boolean): List[A] = fa.filter(p)

      override def takeWhile_[A](fa: List[A])(p: A => Boolean): List[A] = fa.takeWhile(p)

      override def dropWhile_[A](fa: List[A])(p: A => Boolean): List[A] = fa.dropWhile(p)

      override def algebra[A]: Monoid[List[A]] = kernel.instances.ListMonoid[A]

      override def collectFirst[A, B](fa: List[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: List[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

      override def unit: List[Unit] = _unit
      private[this] val _unit: List[Unit] = () :: Nil

      override def void[A](fa: List[A]): List[Unit] = {
        @tailrec
        def build(fa: List[A], acc: List[Unit]): List[Unit] =
          if (fa.isEmpty) acc
          else build(fa.tail, () :: acc)

        // by checking here we can avoid allocating a duplicate unit
        if (fa.isEmpty) Nil
        else build(fa.tail, unit)
      }
    }

  implicit def catsStdShowForList[A: Show]: Show[List[A]] =
    _.iterator.map(Show[A].show).mkString("List(", ", ", ")")

  implicit def catsStdNonEmptyParallelForListZipList: NonEmptyParallel.Aux[List, ZipList] =
    new NonEmptyParallel[List] {
      type F[x] = ZipList[x]

      def flatMap: FlatMap[List] = cats.instances.list.catsStdInstancesForList
      def apply: Apply[ZipList] = ZipList.catsDataCommutativeApplyForZipList

      def sequential: ZipList ~> List =
        new (ZipList ~> List) { def apply[A](a: ZipList[A]): List[A] = a.value }

      def parallel: List ~> ZipList =
        new (List ~> ZipList) { def apply[A](v: List[A]): ZipList[A] = new ZipList(v) }
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
private[instances] trait ListInstancesBinCompat0 {
  implicit val catsStdTraverseFilterForList: TraverseFilter[List] = new TraverseFilter[List] {
    val traverse: Traverse[List] = cats.instances.list.catsStdInstancesForList

    override def mapFilter[A, B](fa: List[A])(f: (A) => Option[B]): List[B] = fa.collect(Function.unlift(f))

    override def filter[A](fa: List[A])(f: (A) => Boolean): List[A] = fa.filter(f)

    override def filterNot[A](fa: List[A])(f: A => Boolean): List[A] = fa.filterNot(f)

    override def collect[A, B](fa: List[A])(f: PartialFunction[A, B]): List[B] = fa.collect(f)

    override def flattenOption[A](fa: List[Option[A]]): List[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: List[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[List[B]] =
      if (fa.isEmpty) G.pure(Nil)
      else
        G match {
          case _ =>
            G.map(Chain.traverseFilterViaChain {
              val as = collection.mutable.ArrayBuffer[A]()
              as ++= fa
              wrapMutableIndexedSeq(as)
            }(f))(_.toList)
        }

    override def filterA[G[_], A](fa: List[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[List[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(List.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, list) => if (b) x :: list else list)
        )
        .value
  }
}
