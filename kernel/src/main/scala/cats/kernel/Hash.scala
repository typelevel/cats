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
import scala.util.hashing.Hashing

/**
 * A type class used to represent a hashing scheme for objects of a given type.
 * For any two instances `x` and `y` that are considered equivalent under the
 * equivalence relation defined by this object, `hash(x)` should equal `hash(y)`.
 * @author Tongfei Chen
 */
trait Hash[@sp A] extends Any with Eq[A] with Serializable { self =>

  /**
   * Returns the hash code of the given object under this hashing scheme.
   */
  def hash(x: A): Int

  // `Hash#toHashing` deliberately not implemented since `scala.util.hashing.Hashing` is only
  // compatible with universal equality.
}

abstract class HashFunctions[H[T] <: Hash[T]] extends EqFunctions[H] {

  def hash[@sp A](x: A)(implicit ev: H[A]): Int = ev.hash(x)

}

object Hash extends HashFunctions[Hash] {

  /**
   * Fetch a `Hash` instance given the specific type.
   */
  @inline final def apply[A](implicit ev: Hash[A]): Hash[A] = ev

  def by[@sp A, @sp B](f: A => B)(implicit ev: Hash[B]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = ev.hash(f(x))
      def eqv(x: A, y: A) = ev.eqv(f(x), f(y))
    }

  def fromHashing[A](implicit ev: Hashing[A]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = ev.hash(x)
      def eqv(x: A, y: A) = x == y // universal equality
    }

  /**
   * Constructs a `Hash` instance by using the universal `hashCode` function and the universal equality relation.
   */
  def fromUniversalHashCode[A]: Hash[A] =
    new Hash[A] {
      def hash(x: A) = x.hashCode()
      def eqv(x: A, y: A) = x == y
    }
}

trait HashToHashingConversion {
  implicit def catsKernelHashToHashing[A](implicit ev: Hash[A]): Hashing[A] =
    ev.hash(_)
}
