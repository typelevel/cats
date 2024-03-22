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

import cats.data.{Chain, Ior, ZipSeq}
import cats.instances.StaticMethods.appendAll
import cats.kernel.compat.scalaVersionSpecific._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.Seq

@suppressUnusedImportWarningForScalaVersionSpecific
trait SeqInstances extends cats.kernel.instances.SeqInstances {
  implicit val catsStdInstancesForSeq
    : Traverse[Seq] with Monad[Seq] with Alternative[Seq] with CoflatMap[Seq] with Align[Seq] =
    new Traverse[Seq] with Monad[Seq] with Alternative[Seq] with CoflatMap[Seq] with Align[Seq] {

      def empty[A]: Seq[A] = Seq.empty[A]

      def combineK[A](x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

      override def combineAllOptionK[A](as: IterableOnce[Seq[A]]): Option[Seq[A]] = {
        val iter = as.iterator
        if (iter.isEmpty) None else Some(appendAll(iter, Seq.newBuilder[A]).result())
      }

      override def fromIterableOnce[A](as: IterableOnce[A]): Seq[A] = {
        val builder = Seq.newBuilder[A]
        builder ++= as
        builder.result()
      }

      override def prependK[A](a: A, fa: Seq[A]): Seq[A] = a +: fa

      override def appendK[A](fa: Seq[A], a: A): Seq[A] = fa :+ a

      def pure[A](x: A): Seq[A] = Seq(x)

      override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] =
        fa.map(f)

      def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Seq[A], fb: Seq[B])(f: (A, B) => Z): Seq[Z] =
        if (fb.isEmpty) Seq.empty // do O(1) work if either is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      private[this] val evalEmpty: Eval[Seq[Nothing]] = Eval.now(Seq.empty)

      override def map2Eval[A, B, Z](fa: Seq[A], fb: Eval[Seq[B]])(f: (A, B) => Z): Eval[Seq[Z]] =
        if (fa.isEmpty) evalEmpty // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def coflatMap[A, B](fa: Seq[A])(f: Seq[A] => B): Seq[B] = {
        @tailrec def loop(builder: mutable.Builder[B, Seq[B]], as: Seq[A]): Seq[B] =
          as match {
            case _ +: rest => loop(builder += f(as), rest)
            case _         => builder.result()
          }
        loop(Seq.newBuilder[B], fa)
      }

      def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Seq[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def tailRecM[A, B](a: A)(fn: A => Seq[Either[A, B]]): Seq[B] = {
        val buf = Seq.newBuilder[B]
        var state = List(fn(a).iterator)
        @tailrec
        def loop(): Unit =
          state match {
            case Nil => ()
            case h :: tail if h.isEmpty =>
              state = tail
              loop()
            case h :: tail =>
              h.next() match {
                case Right(b) =>
                  buf += b
                  loop()
                case Left(a) =>
                  state = (fn(a).iterator) :: h :: tail
                  loop()
              }
          }
        loop()
        buf.result()
      }

      override def size[A](fa: Seq[A]): Long = fa.size.toLong

      override def get[A](fa: Seq[A])(idx: Long): Option[A] =
        if (idx < Int.MaxValue && fa.size > idx && idx >= 0) Some(fa(idx.toInt)) else None

      override def foldMapK[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] = {
        def loop(i: Int): Eval[G[B]] =
          if (i < fa.length) G.combineKEval(f(fa(i)), Eval.defer(loop(i + 1))) else Eval.now(G.empty)
        loop(0).value
      }

      final override def traverse[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Seq[B]] =
        G match {
          case x: StackSafeMonad[G] =>
            x.map(Traverse.traverseDirectly(fa)(f)(x))(_.toList)
          case _ =>
            G.map(Chain.traverseViaChain(fa.toIndexedSeq)(f))(_.toList)
        }

      override def traverse_[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
        G match {
          case x: StackSafeMonad[G] => Traverse.traverse_Directly(fa)(f)(x)
          case _ =>
            foldRight(fa, Eval.now(G.unit)) { (a, acc) =>
              G.map2Eval(f(a), acc) { (_, _) =>
                ()
              }
            }.value
        }

      override def mapWithIndex[A, B](fa: Seq[A])(f: (A, Int) => B): Seq[B] =
        fa.zipWithIndex.map(ai => f(ai._1, ai._2))

      override def zipWithIndex[A](fa: Seq[A]): Seq[(A, Int)] =
        fa.zipWithIndex

      override def exists[A](fa: Seq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Seq[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: Seq[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        val length = fa.length
        G.tailRecM((z, 0)) { case (b, i) =>
          if (i < length) G.map(f(b, fa(i)))(b => Left((b, i + 1)))
          else G.pure(Right(b))
        }
      }

      override def fold[A](fa: Seq[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Seq[A]): List[A] = fa.toList

      override def toIterable[A](fa: Seq[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: Seq[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Seq[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def algebra[A]: Monoid[Seq[A]] = kernel.instances.SeqMonoid[A]

      def functor: Functor[Seq] = this

      def align[A, B](fa: Seq[A], fb: Seq[B]): Seq[A Ior B] = {
        val aLarger = fa.size >= fb.size
        if (aLarger) {
          cats.compat.Seq.zipWith(fa, fb)(Ior.both) ++ fa.drop(fb.size).map(Ior.left)
        } else {
          cats.compat.Seq.zipWith(fa, fb)(Ior.both) ++ fb.drop(fa.size).map(Ior.right)
        }
      }

      override def collectFirst[A, B](fa: Seq[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Seq[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit val catsStdTraverseFilterForSeq: TraverseFilter[Seq] = new TraverseFilter[Seq] {
    val traverse: Traverse[Seq] = cats.instances.seq.catsStdInstancesForSeq

    override def mapFilter[A, B](fa: Seq[A])(f: (A) => Option[B]): Seq[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: Seq[A])(f: (A) => Boolean): Seq[A] = fa.filter(f)

    override def filterNot[A](fa: Seq[A])(f: A => Boolean): Seq[A] = fa.filterNot(f)

    override def collect[A, B](fa: Seq[A])(f: PartialFunction[A, B]): Seq[B] = fa.collect(f)

    override def flattenOption[A](fa: Seq[Option[A]]): Seq[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Seq[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Seq[B]] =
      G match {
        case x: StackSafeMonad[G] => x.map(TraverseFilter.traverseFilterDirectly(fa)(f)(x))(_.toVector)
        case _ =>
          G.map(Chain.traverseFilterViaChain(fa.toIndexedSeq)(f))(_.toVector)
      }

    override def filterA[G[_], A](fa: Seq[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Seq[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Seq.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, Seq) => if (b) x +: Seq else Seq)
        )
        .value
  }

  implicit def catsStdShowForSeq[A: Show]: Show[Seq[A]] =
    _.map(Show[A].show).toString

  implicit def catsStdNonEmptyParallelForSeqZipSeq: NonEmptyParallel.Aux[Seq, ZipSeq] =
    new NonEmptyParallel[Seq] {
      type F[x] = ZipSeq[x]

      def flatMap: FlatMap[Seq] = cats.instances.seq.catsStdInstancesForSeq
      def apply: Apply[ZipSeq] = ZipSeq.catsDataCommutativeApplyForZipSeq

      def sequential: ZipSeq ~> Seq =
        new (ZipSeq ~> Seq) { def apply[A](a: ZipSeq[A]): Seq[A] = a.value }

      def parallel: Seq ~> ZipSeq =
        new (Seq ~> ZipSeq) { def apply[A](v: Seq[A]): ZipSeq[A] = new ZipSeq(v) }
    }
}
