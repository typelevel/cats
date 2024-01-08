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

import scala.{specialized => sp}
import compat.scalaVersionSpecific._

/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
trait Monoid[@sp(Byte, Char, Int, Long, Float, Double) A] extends Any with Semigroup[A] { self =>

  /**
   * Return the identity element for this monoid.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].empty
   * res0: String = ""
   *
   * scala> Monoid[Int].empty
   * res1: Int = 0
   * }}}
   */
  def empty: A

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].isEmpty("")
   * res0: Boolean = true
   *
   * scala> Monoid[String].isEmpty("something")
   * res1: Boolean = false
   * }}}
   */
  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    ev.eqv(a, empty)

  /**
   * Return `a` appended to itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineN("ha", 3)
   * res0: String = hahaha
   *
   * scala> Monoid[String].combineN("ha", 0)
   * res1: String = ""
   * }}}
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineAll(List("One ", "Two ", "Three"))
   * res0: String = One Two Three
   *
   * scala> Monoid[String].combineAll(List.empty)
   * res1: String = ""
   * }}}
   */
  def combineAll(as: IterableOnce[A]): A =
    as.iterator.foldLeft(empty)(combine)

  override def combineAllOption(as: IterableOnce[A]): Option[A] =
    if (as.iterator.isEmpty) None else Some(combineAll(as))

  override def reverse: Monoid[A] =
    new Monoid[A] {
      def empty = self.empty
      def combine(a: A, b: A) = self.combine(b, a)
      // a + a + a + ... is the same when reversed
      override def combineN(a: A, n: Int): A = self.combineN(a, n)
      override def reverse = self
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
abstract class MonoidFunctions[M[T] <: Monoid[T]] extends SemigroupFunctions[M] {
  def empty[@sp(Byte, Char, Int, Long, Float, Double) A](implicit ev: M[A]): A =
    ev.empty

  def isEmpty[@sp(Byte, Char, Int, Long, Float, Double) A](a: A)(implicit m: M[A], ev: Eq[A]): Boolean =
    m.isEmpty(a)

  def combineAll[@sp(Byte, Char, Int, Long, Float, Double) A](as: IterableOnce[A])(implicit ev: M[A]): A =
    ev.combineAll(as)
}

object Monoid extends MonoidFunctions[Monoid] {

  /**
   * Access an implicit `Monoid[A]`.
   */
  @inline final def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev

  /**
   * Create a `Monoid` instance from the given function and empty value.
   */
  @inline def instance[A](emptyValue: A, cmb: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      override val empty: A = emptyValue

      override def combine(x: A, y: A): A = cmb(x, y)
    }
}
