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

package cats.data

import cats.Show
import cats.kernel.{Order, Semigroup}

private[cats] trait NonEmptyCollection[+A, U[+_], NE[+_]] extends Any {
  def head: A
  def tail: U[A]
  def last: A
  def init: U[A]

  def iterator: Iterator[A]

  def map[B](f: A => B): NE[B]
  def reverse: NE[A]
  def prepend[AA >: A](a: AA): NE[AA]
  def append[AA >: A](a: AA): NE[AA]

  def filter(f: A => Boolean): U[A]
  def filterNot(p: A => Boolean): U[A]
  def collect[B](pf: PartialFunction[A, B]): U[B]
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B]
  def find(p: A => Boolean): Option[A]
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean

  def foldLeft[B](b: B)(f: (B, A) => B): B
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA

  def zipWith[B, C](b: NE[B])(f: (A, B) => C): NE[C]
  def zipWithIndex: NE[(A, Int)]

  def distinct[AA >: A](implicit O: Order[AA]): NE[AA]
  def sortBy[B](f: A => B)(implicit B: Order[B]): NE[A]
  def sorted[AA >: A](implicit AA: Order[AA]): NE[AA]
  def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NE[A]]
  def grouped(size: Int): Iterator[NE[A]]
  def toNem[T, V](implicit ev: A <:< (T, V), order: Order[T]): NonEmptyMap[T, V]
  def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B]

  def show[AA >: A](implicit AA: Show[AA]): String
}
