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

package algebra
package ring

import scala.{specialized => sp}
import scala.annotation.{nowarn, tailrec}

trait MultiplicativeSemigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable {
  def multiplicative: Semigroup[A] = times(_, _)

  def times(x: A, y: A): A

  def pow(a: A, n: Int): A =
    if (n > 0) positivePow(a, n)
    else throw new IllegalArgumentException("Illegal non-positive exponent to pow: %s".format(n))

  protected[this] def positivePow(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) times(b, extra)
      else {
        val x = if ((k & 1) == 1) times(b, extra) else extra
        loop(times(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  @nowarn("msg=deprecated")
  def tryProduct(as: TraversableOnce[A]): Option[A] =
    as.toIterator.reduceOption(times)
}

trait MultiplicativeCommutativeSemigroup[@sp(Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: CommutativeSemigroup[A] = times(_, _)
}

trait MultiplicativeMonoid[@sp(Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def empty = one
    def combine(x: A, y: A): A = times(x, y)
  }

  def one: A

  /**
   * Tests if `a` is one.
   */
  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)

  override def pow(a: A, n: Int): A =
    if (n > 0) positivePow(a, n)
    else if (n == 0) one
    else throw new IllegalArgumentException("Illegal negative exponent to pow: %s".format(n))

  /**
   * Given a sequence of `as`, compute the product.
   */
  @nowarn("msg=deprecated")
  def product(as: TraversableOnce[A]): A =
    as.foldLeft(one)(times)

  @nowarn("msg=deprecated")
  override def tryProduct(as: TraversableOnce[A]): Option[A] =
    if (as.isEmpty) None else Some(product(as))
}

trait MultiplicativeCommutativeMonoid[@sp(Int, Long, Float, Double) A]
    extends Any
    with MultiplicativeMonoid[A]
    with MultiplicativeCommutativeSemigroup[A] {
  override def multiplicative: CommutativeMonoid[A] = new CommutativeMonoid[A] {
    def empty = one
    def combine(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeGroup[@sp(Int, Long, Float, Double) A] extends Any with MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def empty = one
    def combine(x: A, y: A): A = times(x, y)
    override def remove(x: A, y: A): A = div(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A

  override def pow(a: A, n: Int): A =
    if (n > 0) positivePow(a, n)
    else if (n == 0) one
    else if (n == Int.MinValue) positivePow(reciprocal(times(a, a)), 1073741824)
    else positivePow(reciprocal(a), -n)
}

trait MultiplicativeCommutativeGroup[@sp(Int, Long, Float, Double) A]
    extends Any
    with MultiplicativeGroup[A]
    with MultiplicativeCommutativeMonoid[A] {
  override def multiplicative: CommutativeGroup[A] = new CommutativeGroup[A] {
    def empty = one
    def combine(x: A, y: A): A = times(x, y)
    override def remove(x: A, y: A): A = div(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

trait MultiplicativeSemigroupFunctions[S[T] <: MultiplicativeSemigroup[T]] {
  def isMultiplicativeCommutative[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[MultiplicativeCommutativeSemigroup[A]]

  def times[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.times(x, y)
  def pow[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: S[A]): A =
    ev.pow(a, n)

  @nowarn("msg=deprecated")
  def tryProduct[A](as: TraversableOnce[A])(implicit ev: S[A]): Option[A] =
    ev.tryProduct(as)
}

trait MultiplicativeMonoidFunctions[M[T] <: MultiplicativeMonoid[T]] extends MultiplicativeSemigroupFunctions[M] {
  def one[@sp(Int, Long, Float, Double) A](implicit ev: M[A]): A =
    ev.one

  def isOne[@sp(Int, Long, Float, Double) A](a: A)(implicit ev0: M[A], ev1: Eq[A]): Boolean =
    ev0.isOne(a)

  @nowarn("msg=deprecated")
  def product[@sp(Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: M[A]): A =
    ev.product(as)
}

trait MultiplicativeGroupFunctions[G[T] <: MultiplicativeGroup[T]] extends MultiplicativeMonoidFunctions[G] {
  def reciprocal[@sp(Int, Long, Float, Double) A](x: A)(implicit ev: G[A]): A =
    ev.reciprocal(x)
  def div[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: G[A]): A =
    ev.div(x, y)
}

object MultiplicativeSemigroup extends MultiplicativeSemigroupFunctions[MultiplicativeSemigroup] {
  @inline final def apply[A](implicit ev: MultiplicativeSemigroup[A]): MultiplicativeSemigroup[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeSemigroup[A]`, this method returns
   * a `Semigroup[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeSemigroup[A]): Semigroup[A] =
    ev.multiplicative
}

object MultiplicativeCommutativeSemigroup extends MultiplicativeSemigroupFunctions[MultiplicativeCommutativeSemigroup] {
  @inline final def apply[A](implicit
    ev: MultiplicativeCommutativeSemigroup[A]
  ): MultiplicativeCommutativeSemigroup[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeCommutativeSemigroup[A]`, this method returns
   * a `CommutativeSemigroup[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeCommutativeSemigroup[A]): CommutativeSemigroup[A] =
    ev.multiplicative
}

object MultiplicativeMonoid extends MultiplicativeMonoidFunctions[MultiplicativeMonoid] {
  @inline final def apply[A](implicit ev: MultiplicativeMonoid[A]): MultiplicativeMonoid[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeMonoid[A]`, this method returns
   * a `Monoid[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeMonoid[A]): Monoid[A] =
    ev.multiplicative
}

object MultiplicativeCommutativeMonoid extends MultiplicativeMonoidFunctions[MultiplicativeCommutativeMonoid] {
  @inline final def apply[A](implicit ev: MultiplicativeCommutativeMonoid[A]): MultiplicativeCommutativeMonoid[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeCommutativeMonoid[A]`, this method returns
   * a `CommutativeMonoid[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeCommutativeMonoid[A]): CommutativeMonoid[A] =
    ev.multiplicative
}

object MultiplicativeGroup extends MultiplicativeGroupFunctions[MultiplicativeGroup] {
  @inline final def apply[A](implicit ev: MultiplicativeGroup[A]): MultiplicativeGroup[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeGroup[A]`, this method returns
   * a `Group[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeGroup[A]): Group[A] =
    ev.multiplicative
}

object MultiplicativeCommutativeGroup extends MultiplicativeGroupFunctions[MultiplicativeCommutativeGroup] {
  @inline final def apply[A](implicit ev: MultiplicativeCommutativeGroup[A]): MultiplicativeCommutativeGroup[A] = ev

  /**
   * This method converts a multiplicative instance into a generic
   * instance.
   *
   * Given an implicit `MultiplicativeCommutativeGroup[A]`, this method returns
   * a `CommutativeGroup[A]`.
   */
  @inline final def multiplicative[A](implicit ev: MultiplicativeCommutativeGroup[A]): CommutativeGroup[A] =
    ev.multiplicative
}
