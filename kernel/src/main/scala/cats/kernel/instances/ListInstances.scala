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

import scala.annotation.{nowarn, tailrec}
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait ListInstances extends ListInstances1 {
  implicit def catsKernelStdOrderForList[A: Order]: Order[List[A]] =
    new ListOrder[A]

  implicit def catsKernelStdMonoidForList[A]: Monoid[List[A]] =
    ListMonoid[A]
}

private[instances] trait ListInstances1 extends ListInstances2 {
  implicit def catsKernelStdPartialOrderForList[A: PartialOrder]: PartialOrder[List[A]] =
    new ListPartialOrder[A]

  implicit def catsKernelStdHashForList[A: Hash]: Hash[List[A]] =
    new ListHash[A]
}

private[instances] trait ListInstances2 {
  implicit def catsKernelStdEqForList[A: Eq]: Eq[List[A]] =
    new ListEq[A]
}

class ListOrder[A](implicit ev: Order[A]) extends Order[List[A]] {
  def compare(xs: List[A], ys: List[A]): Int = {
    @tailrec def loop(xs: List[A], ys: List[A]): Int =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0 else -1
        case x :: xs =>
          ys match {
            case Nil => 1
            case y :: ys =>
              val n = ev.compare(x, y)
              if (n != 0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0 else loop(xs, ys)
  }
}

class ListPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[List[A]] {
  def partialCompare(xs: List[A], ys: List[A]): Double = {
    @tailrec def loop(xs: List[A], ys: List[A]): Double =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0.0 else -1.0
        case x :: xs =>
          ys match {
            case Nil => 1.0
            case y :: ys =>
              val n = ev.partialCompare(x, y)
              if (n != 0.0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0.0 else loop(xs, ys)
  }
}

class ListHash[A](implicit ev: Hash[A]) extends ListEq[A]()(ev) with Hash[List[A]] {
  def hash(x: List[A]): Int = StaticMethods.listHash(x)(ev)
}

class ListEq[A](implicit ev: Eq[A]) extends Eq[List[A]] {
  def eqv(xs: List[A], ys: List[A]): Boolean = {
    def loop(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil =>
          ys.isEmpty
        case x :: xs =>
          ys match {
            case y :: ys =>
              if (ev.eqv(x, y)) loop(xs, ys) else false
            case Nil =>
              false
          }
      }
    (xs eq ys) || loop(xs, ys)
  }
}

@deprecated("Use ListMonoid.apply, which does not allocate a new instance", "2.9.0")
class ListMonoid[A] extends Monoid[List[A]] { self =>
  def empty: List[A] = Nil
  def combine(x: List[A], y: List[A]): List[A] = x ::: y

  override def combineN(x: List[A], n: Int): List[A] =
    StaticMethods.combineNIterable(List.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[List[A]]): List[A] =
    StaticMethods.combineAllIterable(List.newBuilder[A], xs)

  override def reverse: Monoid[List[A]] =
    new Monoid[List[A]] {
      def empty: List[A] = Nil
      def combine(x: List[A], y: List[A]) = y ::: x

      override def combineAll(xs: IterableOnce[List[A]]): List[A] =
        xs.iterator.foldLeft(empty) { (acc, item) =>
          item ::: acc
        }

      override def reverse: Monoid[List[A]] = self
    }
}

object ListMonoid {
  @nowarn("msg=deprecated")
  private[this] val singleton: Monoid[List[Any]] = new ListMonoid[Any]
  def apply[A]: Monoid[List[A]] = singleton.asInstanceOf[Monoid[List[A]]]
}
