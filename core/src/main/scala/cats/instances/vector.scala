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

import cats.data.{Chain, Ior, ZipVector}
import cats.instances.StaticMethods.appendAll
import cats.kernel.compat.scalaVersionSpecific._

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

trait VectorInstances extends cats.kernel.instances.VectorInstances {
  implicit val catsStdInstancesForVector
    : Traverse[Vector] with Monad[Vector] with Alternative[Vector] with CoflatMap[Vector] with Align[Vector] =
    new Traverse[Vector] with Monad[Vector] with Alternative[Vector] with CoflatMap[Vector] with Align[Vector] {

      def empty[A]: Vector[A] = Vector.empty[A]

      def combineK[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

      override def combineAllOptionK[A](as: IterableOnce[Vector[A]]): Option[Vector[A]] = {
        val iter = as.iterator
        if (iter.isEmpty) None else Some(appendAll(iter, Vector.newBuilder[A]).result())
      }

      override def fromIterableOnce[A](as: IterableOnce[A]): Vector[A] =
        as.iterator.toVector

      override def prependK[A](a: A, fa: Vector[A]): Vector[A] = a +: fa

      override def appendK[A](fa: Vector[A], a: A): Vector[A] = fa :+ a

      def pure[A](x: A): Vector[A] = Vector(x)

      override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
        fa.map(f)

      def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Vector[A], fb: Vector[B])(f: (A, B) => Z): Vector[Z] =
        if (fb.isEmpty) Vector.empty // do O(1) work if either is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      private[this] val evalEmpty: Eval[Vector[Nothing]] = Eval.now(Vector.empty)

      override def map2Eval[A, B, Z](fa: Vector[A], fb: Eval[Vector[B]])(f: (A, B) => Z): Eval[Vector[Z]] =
        if (fa.isEmpty) evalEmpty // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def coflatMap[A, B](fa: Vector[A])(f: Vector[A] => B): Vector[B] = {
        @tailrec def loop(builder: VectorBuilder[B], as: Vector[A]): Vector[B] =
          as match {
            case _ +: rest => loop(builder += f(as), rest)
            case _         => builder.result()
          }
        loop(new VectorBuilder[B], fa)
      }

      def foldLeft[A, B](fa: Vector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Vector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(i: Int): Eval[B] =
          if (i < fa.length) f(fa(i), Eval.defer(loop(i + 1))) else lb
        Eval.defer(loop(0))
      }

      override def foldMap[A, B](fa: Vector[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def tailRecM[A, B](a: A)(fn: A => Vector[Either[A, B]]): Vector[B] = {
        val buf = Vector.newBuilder[B]
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

      override def size[A](fa: Vector[A]): Long = fa.size.toLong

      override def get[A](fa: Vector[A])(idx: Long): Option[A] =
        if (idx < Int.MaxValue && fa.size > idx && idx >= 0) Some(fa(idx.toInt)) else None

      override def foldMapK[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] = {
        def loop(i: Int): Eval[G[B]] =
          if (i < fa.length) G.combineKEval(f(fa(i)), Eval.defer(loop(i + 1))) else Eval.now(G.empty)
        loop(0).value
      }

      final override def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
        G match {
          case _ => G.map(Chain.traverseViaChain(fa)(f))(_.toVector)
        }

      final override def updated_[A, B >: A](fa: Vector[A], idx: Long, b: B): Option[Vector[B]] =
        if (idx >= 0L && idx < fa.size.toLong) {
          Some(fa.updated(idx.toInt, b))
        } else {
          None
        }

      /**
       * This avoids making a very deep stack by building a tree instead
       */
      override def traverse_[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = {
        G match {
          case _ =>
            // the cost of this is O(size)
            // c(n) = 1 + 2 * c(n/2)
            // invariant: size >= 1
            def runHalf(size: Int, idx: Int): Eval[G[Unit]] =
              if (size > 1) {
                val leftSize = size / 2
                val rightSize = size - leftSize
                runHalf(leftSize, idx)
                  .flatMap { left =>
                    val right = runHalf(rightSize, idx + leftSize)
                    G.map2Eval(left, right) { (_, _) => () }
                  }
              } else {
                val a = fa(idx)
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
            else runHalf(len, 0).value
        }
      }

      override def mapAccumulate[S, A, B](init: S, fa: Vector[A])(f: (S, A) => (S, B)): (S, Vector[B]) =
        StaticMethods.mapAccumulateFromStrictFunctor(init, fa, f)(this)

      override def mapWithIndex[A, B](fa: Vector[A])(f: (A, Int) => B): Vector[B] =
        StaticMethods.mapWithIndexFromStrictFunctor(fa, f)(this)

      override def mapWithLongIndex[A, B](fa: Vector[A])(f: (A, Long) => B): Vector[B] =
        StaticMethods.mapWithLongIndexFromStrictFunctor(fa, f)(this)

      override def zipWithIndex[A](fa: Vector[A]): Vector[(A, Int)] =
        fa.zipWithIndex

      override def exists[A](fa: Vector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Vector[A]): Boolean =
        fa.isEmpty

      override def foldM[G[_], A, B](fa: Vector[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        val length = fa.length
        G.tailRecM((z, 0)) { case (b, i) =>
          if (i < length) G.map(f(b, fa(i)))(b => Left((b, i + 1)))
          else G.pure(Right(b))
        }
      }

      override def fold[A](fa: Vector[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Vector[A]): List[A] = fa.toList

      override def toIterable[A](fa: Vector[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: Vector[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Vector[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def algebra[A]: Monoid[Vector[A]] = kernel.instances.VectorMonoid[A]

      def functor: Functor[Vector] = this

      def align[A, B](fa: Vector[A], fb: Vector[B]): Vector[A Ior B] = {
        val aLarger = fa.size >= fb.size
        if (aLarger) {
          cats.compat.Vector.zipWith(fa, fb)(Ior.both) ++ fa.drop(fb.size).map(Ior.left)
        } else {
          cats.compat.Vector.zipWith(fa, fb)(Ior.both) ++ fb.drop(fa.size).map(Ior.right)
        }
      }

      override def collectFirst[A, B](fa: Vector[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Vector[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit def catsStdShowForVector[A: Show]: Show[Vector[A]] =
    _.iterator.map(Show[A].show).mkString("Vector(", ", ", ")")

  implicit def catsStdNonEmptyParallelForVectorZipVector: NonEmptyParallel.Aux[Vector, ZipVector] =
    new NonEmptyParallel[Vector] {
      type F[x] = ZipVector[x]

      def flatMap: FlatMap[Vector] = cats.instances.vector.catsStdInstancesForVector
      def apply: Apply[ZipVector] = ZipVector.catsDataCommutativeApplyForZipVector

      def sequential: ZipVector ~> Vector =
        new (ZipVector ~> Vector) { def apply[A](a: ZipVector[A]): Vector[A] = a.value }

      def parallel: Vector ~> ZipVector =
        new (Vector ~> ZipVector) { def apply[A](v: Vector[A]): ZipVector[A] = new ZipVector(v) }
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
private[instances] trait VectorInstancesBinCompat0 {
  implicit val catsStdTraverseFilterForVector: TraverseFilter[Vector] = new TraverseFilter[Vector] {
    val traverse: Traverse[Vector] = cats.instances.vector.catsStdInstancesForVector

    override def mapFilter[A, B](fa: Vector[A])(f: (A) => Option[B]): Vector[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: Vector[A])(f: (A) => Boolean): Vector[A] = fa.filter(f)

    override def filterNot[A](fa: Vector[A])(f: A => Boolean): Vector[A] = fa.filterNot(f)

    override def collect[A, B](fa: Vector[A])(f: PartialFunction[A, B]): Vector[B] = fa.collect(f)

    override def flattenOption[A](fa: Vector[Option[A]]): Vector[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Vector[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Vector[B]] =
      G match {
        case _ =>
          G.map(Chain.traverseFilterViaChain(fa)(f))(_.toVector)
      }

    override def filterA[G[_], A](fa: Vector[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Vector[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Vector.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, vector) => if (b) x +: vector else vector)
        )
        .value
  }
}
