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
import cats.instances.StaticMethods.{appendAll, appendAllMixed}
import cats.kernel.compat.scalaVersionSpecific.*

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.mutable.Builder

trait IArrayInstances extends cats.kernel.instances.IArrayInstances {
  implicit val catsStdInstancesForIArray
    : Traverse[IArray] & Monad[IArray] & Alternative[IArray] & CoflatMap[IArray] & Align[IArray] =
    new Traverse[IArray] with Monad[IArray] with Alternative[IArray] with CoflatMap[IArray] with Align[IArray] {

      def empty[A]: IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        IArray.empty[A]
      }

      def combineK[A](x: IArray[A], y: IArray[A]): IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        x.appendedAll(y)
      }

      override def combineAllOptionK[A](as: IterableOnce[IArray[A]]): Option[IArray[A]] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        val iter = as.iterator
        if iter.isEmpty then None else Some(appendAllMixed(iter.map(_.iterator), IArray.newBuilder[A]).result())
      }

      override def fromIterableOnce[A](as: IterableOnce[A]): IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        IArray.unsafeFromArray(as.iterator.toArray)
      }

      override def prependK[A](a: A, fa: IArray[A]): IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        fa.prepended(a)
      }

      override def appendK[A](fa: IArray[A], a: A): IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        fa.appended(a)
      }

      def pure[A](x: A): IArray[A] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        IArray(x)
      }



      override def map[A, B](fa: IArray[A])(f: A => B): IArray[B] = {
        implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
        fa.map(f)
      }

      def flatMap[A, B](fa: IArray[A])(f: A => IArray[B]): IArray[B] = {
        implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
        fa.flatMap(f.andThen(_.iterator))
      }

      override def map2[A, B, Z](fa: IArray[A], fb: IArray[B])(f: (A, B) => Z): IArray[Z] = {
        implicit val fakeClassTag: ClassTag[Z] = summonFakeTag[Z]
        if (fb.isEmpty) IArray.empty[Z] // do O(1) work if either is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty
      }

      private[this] val evalEmpty: Eval[IArray[Nothing]] = Eval.now(IArray.empty(ClassTag.Nothing))
      private[this] def summonFakeTag[T]: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]]

      override def map2Eval[A, B, Z](fa: IArray[A], fb: Eval[IArray[B]])(f: (A, B) => Z): Eval[IArray[Z]] =
        if fa.isEmpty then evalEmpty // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def coflatMap[A, B](fa: IArray[A])(f: IArray[A] => B): IArray[B] = {
        implicit val fakeClassTag: ClassTag[B] = summonFakeTag[B]
        @tailrec def loop(builder: Builder[B, IArray[B]], as: IArray[A]): IArray[B] = {
          if as.nonEmpty then
            loop(builder += f(as), as.tail)
          else
            builder.result()
        }

        loop(IArray.newBuilder[B], fa)
      }

      def foldLeft[A, B](fa: IArray[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: IArray[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(i: Int): Eval[B] =
          if i < fa.length then f(fa(i), Eval.defer(loop(i + 1))) else lb
        Eval.defer(loop(0))
      }

      override def foldMap[A, B](fa: IArray[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def tailRecM[A, B](a: A)(fn: A => IArray[Either[A, B]]): IArray[B] = {
        implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
        val buf = IArray.newBuilder[B]
        var state = List(fn(a).iterator)
        @tailrec
        def loop(): Unit =
          state match {
            case Nil                    => ()
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

      override def size[A](fa: IArray[A]): Long = fa.size.toLong

      override def get[A](fa: IArray[A])(idx: Long): Option[A] =
        if idx < Int.MaxValue && fa.size > idx && idx >= 0 then Some(fa(idx.toInt)) else None

      override def foldMapK[G[_], A, B](fa: IArray[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] = {
        def loop(i: Int): Eval[G[B]] =
          if i < fa.length then G.combineKEval(f(fa(i)), Eval.defer(loop(i + 1))) else Eval.now(G.empty)
        loop(0).value
      }

      final override def traverse[G[_], A, B](fa: IArray[A])(f: A => G[B])(implicit G: Applicative[G]): G[IArray[B]] = {
        implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
        G match {
          case x: StackSafeMonad[G] =>
            x.map(Traverse.traverseDirectly(fa)(f)(x))(it => IArray.unsafeFromArray(it.toVector.toArray))
          case _ => G.map(Chain.traverseViaChain(fa)(f))(it =>  IArray.unsafeFromArray(it.toVector.toArray))
        }
      }

      final override def updated_[A, B >: A](fa: IArray[A], idx: Long, b: B): Option[IArray[B]] = {
        implicit val fakeClassTag: ClassTag[B] = summonFakeTag[B]
        if idx >= 0L && idx < fa.size.toLong then {
          Some(fa.updated(idx.toInt, b))
        } else {
          None
        }
      }

      /**
       * This avoids making a very deep stack by building a tree instead
       */
      override def traverseVoid[G[_], A, B](fa: IArray[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = {
        implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
        G match {
          case x: StackSafeMonad[G] => Traverse.traverseVoidDirectly(fa)(f)(x)
          case _                    =>
            // the cost of this is O(size)
            // c(n) = 1 + 2 * c(n/2)
            // invariant: size >= 1
            def runHalf(size: Int, idx: Int): Eval[G[Unit]] =
              if size > 1 then {
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
            if len == 0 then G.unit
            else runHalf(len, 0).value
        }
      }

      override def mapAccumulate[S, A, B](init: S, fa: IArray[A])(f: (S, A) => (S, B)): (S, IArray[B]) =
        StaticMethods.mapAccumulateFromStrictFunctor(init, fa, f)(this)

      override def mapWithIndex[A, B](fa: IArray[A])(f: (A, Int) => B): IArray[B] =
        StaticMethods.mapWithIndexFromStrictFunctor(fa, f)(this)

      override def mapWithLongIndex[A, B](fa: IArray[A])(f: (A, Long) => B): IArray[B] =
        StaticMethods.mapWithLongIndexFromStrictFunctor(fa, f)(this)

      override def zipWithIndex[A](fa: IArray[A]): IArray[(A, Int)] =
        fa.zipWithIndex

      override def exists[A](fa: IArray[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: IArray[A]): Boolean =
        fa.isEmpty

      override def foldM[G[_], A, B](fa: IArray[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        val length = fa.length
        G.tailRecM((z, 0)) { case (b, i) =>
          if i < length then G.map(f(b, fa(i)))(b => Left((b, i + 1)))
          else G.pure(Right(b))
        }
      }

      override def fold[A](fa: IArray[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: IArray[A]): List[A] = fa.toList

      override def toIterable[A](fa: IArray[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: IArray[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: IArray[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def algebra[A]: Monoid[IArray[A]] = {
        implicit val fakeClassTag: ClassTag[A] = summonFakeTag[A]
        kernel.instances.IArrayMonoid[A]
      }

      def functor: Functor[IArray] = this

      def align[A, B](fa: IArray[A], fb: IArray[B]): IArray[A Ior B] = {
        val aLarger = fa.size >= fb.size
        val faMut = fa.asInstanceOf[Array[A]]
        val fbMut = fb.asInstanceOf[Array[B]]
        val prefix = faMut.lazyZip(fbMut).map(Ior.both).asInstanceOf[IArray[A Ior B]]
        if aLarger then {
          prefix ++ fa.drop(fb.size).map(Ior.left)
        } else {
          prefix ++ fb.drop(fa.size).map(Ior.right)
        }
      }

      override def collectFirst[A, B](fa: IArray[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: IArray[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit def catsStdShowForIArray[A: Show]: Show[IArray[A]] =
    _.iterator.map(Show[A].show).mkString("IArray(", ", ", ")")

  implicit val catsStdTraverseFilterForIArray: TraverseFilter[IArray] = new TraverseFilter[IArray] {
    val traverse: Traverse[IArray] = cats.instances.iarray.catsStdInstancesForIArray

    override def mapFilter[A, B](fa: IArray[A])(f: (A) => Option[B]): IArray[B] = {
      implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
      fa.collect(Function.unlift(f))
    }

    override def filter[A](fa: IArray[A])(f: (A) => Boolean): IArray[A] = fa.filter(f)

    override def filterNot[A](fa: IArray[A])(f: A => Boolean): IArray[A] = fa.filterNot(f)

    override def collect[A, B](fa: IArray[A])(f: PartialFunction[A, B]): IArray[B] = {
      implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
      fa.collect(f)
    }

    override def flattenOption[A](fa: IArray[Option[A]]): IArray[A] = {
      implicit val fakeClassTag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]]
      IArray.flatten(fa)
    }



    def traverseFilter[G[_], A, B](
                                    fa: IArray[A]
                                  )(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[IArray[B]] = {
      implicit val fakeClassTag: ClassTag[B] = ClassTag.Any.asInstanceOf[ClassTag[B]]
      G match {
        case x: StackSafeMonad[G] =>
          x.map(TraverseFilter.traverseFilterDirectly(fa)(f)(x))(it => IArray.unsafeFromArray(it.toVector.toArray))
        case _ =>
          G.map(Chain.traverseFilterViaChain(fa)(f))(it => IArray.unsafeFromArray(it.toVector.toArray))
      }
    }

    override def filterA[G[_], A](
                                   fa: IArray[A]
                                 )(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[IArray[A]] = {
      implicit val fakeClassTag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]]
      traverse
        .foldRight(fa, Eval.now(G.pure(IArray.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, IArray) => if b then x +: IArray else IArray)
        )
        .value
    }
  }

  /*
  implicit def catsStdNonEmptyParallelForIArrayZipIArray: NonEmptyParallel.Aux[IArray, ZipIArray] =
    new NonEmptyParallel[IArray] {
      type F[x] = ZipIArray[x]

      def flatMap: FlatMap[IArray] = cats.instances.IArray.catsStdInstancesForIArray
      def apply: Apply[ZipIArray] = ZipIArray.catsDataCommutativeApplyForZipIArray

      def sequential: ZipIArray ~> IArray =
        new (ZipIArray ~> IArray) { def apply[A](a: ZipIArray[A]): IArray[A] = a.value }

      def parallel: IArray ~> ZipIArray =
        new (IArray ~> ZipIArray) { def apply[A](v: IArray[A]): ZipIArray[A] = new ZipIArray(v) }
    }
   */
}
