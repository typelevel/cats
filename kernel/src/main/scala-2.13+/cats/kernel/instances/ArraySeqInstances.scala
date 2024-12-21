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

package cats.kernel
package instances

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import compat.scalaVersionSpecific.*

@suppressUnusedImportWarningForScalaVersionSpecific
trait ArraySeqInstances extends ArraySeqInstances.ArraySeqInstances1 {
  implicit def catsKernelStdOrderForArraySeq[A: Order]: Order[ArraySeq[A]] =
    new ArraySeqInstances.ArraySeqOrder[A]

  implicit def catsKernelStdMonoidForArraySeq[A]: Monoid[ArraySeq[A]] =
    new ArraySeqInstances.ArraySeqMonoid[A]
}

object ArraySeqInstances {
  private[instances] trait ArraySeqInstances1 extends ArraySeqInstances2 {
    implicit def catsKernelStdPartialOrderForArraySeq[A: PartialOrder]: PartialOrder[ArraySeq[A]] =
      new ArraySeqPartialOrder[A]

    implicit def catsKernelStdHashForArraySeq[A: Hash]: Hash[ArraySeq[A]] =
      new ArraySeqHash[A]
  }

  private[instances] trait ArraySeqInstances2 {
    implicit def catsKernelStdEqForArraySeq[A: Eq]: Eq[ArraySeq[A]] =
      new ArraySeqEq[A]
  }

  final private class ArraySeqOrder[A](implicit ev: Order[A]) extends Order[ArraySeq[A]] {
    final def compare(xs: ArraySeq[A], ys: ArraySeq[A]): Int = {
      @tailrec def loop(i: Int): Int =
        (i < xs.length, i < ys.length) match {
          case (true, true) =>
            val n = ev.compare(xs(i), ys(i))
            if (n != 0) n else loop(i + 1)
          case (true, false)  => 1
          case (false, true)  => -1
          case (false, false) => 0
        }

      if (xs eq ys) 0 else loop(i = 0)
    }
  }

  private class ArraySeqPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[ArraySeq[A]] {
    final def partialCompare(xs: ArraySeq[A], ys: ArraySeq[A]): Double = {
      @tailrec def loop(i: Int): Double =
        (i < xs.length, i < ys.length) match {
          case (true, true) =>
            val n = ev.partialCompare(xs(i), ys(i))
            if (n != 0) n else loop(i + 1)
          case (true, false)  => 1
          case (false, true)  => -1
          case (false, false) => 0
        }

      if (xs eq ys) 0.0 else loop(i = 0)
    }
  }

  private class ArraySeqHash[A](implicit ev: Hash[A]) extends ArraySeqEq[A]()(ev) with Hash[ArraySeq[A]] {
    final def hash(xs: ArraySeq[A]): Int = StaticMethods.orderedHash(xs)
  }

  private class ArraySeqEq[A](implicit ev: Eq[A]) extends Eq[ArraySeq[A]] {
    final def eqv(xs: ArraySeq[A], ys: ArraySeq[A]): Boolean = {
      @tailrec def loop(i: Int): Boolean =
        (i < xs.length, i < ys.length) match {
          case (true, true)   => if (ev.eqv(xs(i), ys(i))) loop(i + 1) else false
          case (true, false)  => false
          case (false, true)  => false
          case (false, false) => true
        }

      (xs eq ys) || loop(i = 0)
    }
  }

  final private[cats] class ArraySeqMonoid[A] extends Monoid[ArraySeq[A]] {
    def empty: ArraySeq[A] =
      ArraySeq.untagged.empty

    def combine(xs: ArraySeq[A], ys: ArraySeq[A]): ArraySeq[A] =
      xs.concat(ys)

    override def combineN(x: ArraySeq[A], n: Int): ArraySeq[A] =
      StaticMethods.combineNIterable(ArraySeq.untagged.newBuilder[A], x, n)

    override def combineAll(xs: IterableOnce[ArraySeq[A]]): ArraySeq[A] =
      StaticMethods.combineAllIterable(ArraySeq.untagged.newBuilder[A], xs)
  }
}
