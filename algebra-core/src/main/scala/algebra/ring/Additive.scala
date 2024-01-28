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

trait AdditiveSemigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable {
  def additive: Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
  }

  def plus(x: A, y: A): A

  def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else throw new IllegalArgumentException("Illegal non-positive exponent to sumN: %s".format(n))

  protected[this] def positiveSumN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) plus(b, extra)
      else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  @nowarn("msg=deprecated")
  def trySum(as: TraversableOnce[A]): Option[A] =
    as.toIterator.reduceOption(plus)
}

trait AdditiveCommutativeSemigroup[@sp(Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
  }
}

trait AdditiveMonoid[@sp(Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
    @nowarn("msg=deprecated")
    override def combineAll(as: TraversableOnce[A]): A = sum(as)
  }

  def zero: A

  /**
   * Tests if `a` is zero.
   */
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)

  override def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else throw new IllegalArgumentException("Illegal negative exponent to sumN: %s".format(n))

  /**
   * Given a sequence of `as`, compute the sum.
   */
  @nowarn("msg=deprecated")
  def sum(as: TraversableOnce[A]): A =
    as.foldLeft(zero)(plus)

  @nowarn("msg=deprecated")
  override def trySum(as: TraversableOnce[A]): Option[A] =
    if (as.isEmpty) None else Some(sum(as))
}

trait AdditiveCommutativeMonoid[@sp(Int, Long, Float, Double) A]
    extends Any
    with AdditiveMonoid[A]
    with AdditiveCommutativeSemigroup[A] {
  override def additive: CommutativeMonoid[A] = new CommutativeMonoid[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
    @nowarn("msg=deprecated")
    override def combineAll(as: TraversableOnce[A]): A = sum(as)
  }
}

trait AdditiveGroup[@sp(Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    override def remove(x: A, y: A): A = minus(x, y)
    def inverse(x: A): A = negate(x)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
    @nowarn("msg=deprecated")
    override def combineAll(as: TraversableOnce[A]): A = sum(as)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))

  override def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else if (n == Int.MinValue) positiveSumN(negate(plus(a, a)), 1073741824)
    else positiveSumN(negate(a), -n)
}

trait AdditiveCommutativeGroup[@sp(Int, Long, Float, Double) A]
    extends Any
    with AdditiveGroup[A]
    with AdditiveCommutativeMonoid[A] {
  override def additive: CommutativeGroup[A] = new CommutativeGroup[A] {
    def empty = zero
    def combine(x: A, y: A): A = plus(x, y)
    override def remove(x: A, y: A): A = minus(x, y)
    def inverse(x: A): A = negate(x)
    @nowarn("msg=deprecated")
    override def combineAllOption(as: TraversableOnce[A]): Option[A] = trySum(as)
    @nowarn("msg=deprecated")
    override def combineAll(as: TraversableOnce[A]): A = sum(as)
  }
}

trait AdditiveSemigroupFunctions[S[T] <: AdditiveSemigroup[T]] {

  def isAdditiveCommutative[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[AdditiveCommutativeSemigroup[?]]

  def plus[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.plus(x, y)

  def sumN[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: S[A]): A =
    ev.sumN(a, n)

  @nowarn("msg=deprecated")
  def trySum[A](as: TraversableOnce[A])(implicit ev: S[A]): Option[A] =
    ev.trySum(as)
}

trait AdditiveMonoidFunctions[M[T] <: AdditiveMonoid[T]] extends AdditiveSemigroupFunctions[M] {
  def zero[@sp(Int, Long, Float, Double) A](implicit ev: M[A]): A =
    ev.zero

  def isZero[@sp(Int, Long, Float, Double) A](a: A)(implicit ev0: M[A], ev1: Eq[A]): Boolean =
    ev0.isZero(a)

  @nowarn("msg=deprecated")
  def sum[@sp(Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: M[A]): A =
    ev.sum(as)
}

trait AdditiveGroupFunctions[G[T] <: AdditiveGroup[T]] extends AdditiveMonoidFunctions[G] {
  def negate[@sp(Int, Long, Float, Double) A](x: A)(implicit ev: G[A]): A =
    ev.negate(x)
  def minus[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: G[A]): A =
    ev.minus(x, y)
}

object AdditiveSemigroup extends AdditiveSemigroupFunctions[AdditiveSemigroup] {
  @inline final def apply[A](implicit ev: AdditiveSemigroup[A]): AdditiveSemigroup[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveSemigroup[A]`, this method returns a
   * `Semigroup[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveSemigroup[A]): Semigroup[A] =
    ev.additive
}

object AdditiveCommutativeSemigroup extends AdditiveSemigroupFunctions[AdditiveCommutativeSemigroup] {
  @inline final def apply[A](implicit ev: AdditiveCommutativeSemigroup[A]): AdditiveCommutativeSemigroup[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveCommutativeSemigroup[A]`, this method returns a
   * `CommutativeSemigroup[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveCommutativeSemigroup[A]): CommutativeSemigroup[A] =
    ev.additive
}

object AdditiveMonoid extends AdditiveMonoidFunctions[AdditiveMonoid] {
  @inline final def apply[A](implicit ev: AdditiveMonoid[A]): AdditiveMonoid[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveMonoid[A]`, this method returns a
   * `Monoid[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveMonoid[A]): Monoid[A] =
    ev.additive
}

object AdditiveCommutativeMonoid extends AdditiveMonoidFunctions[AdditiveCommutativeMonoid] {
  @inline final def apply[A](implicit ev: AdditiveCommutativeMonoid[A]): AdditiveCommutativeMonoid[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveCommutativeMonoid[A]`, this method returns a
   * `CommutativeMonoid[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveCommutativeMonoid[A]): CommutativeMonoid[A] =
    ev.additive
}

object AdditiveGroup extends AdditiveGroupFunctions[AdditiveGroup] {
  @inline final def apply[A](implicit ev: AdditiveGroup[A]): AdditiveGroup[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveGroup[A]`, this method returns a
   * `Group[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveGroup[A]): Group[A] =
    ev.additive
}

object AdditiveCommutativeGroup extends AdditiveGroupFunctions[AdditiveCommutativeGroup] {
  @inline final def apply[A](implicit ev: AdditiveCommutativeGroup[A]): AdditiveCommutativeGroup[A] = ev

  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveCommutativeGroup[A]`, this method returns a
   * `CommutativeGroup[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveCommutativeGroup[A]): CommutativeGroup[A] =
    ev.additive
}
