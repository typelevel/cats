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

trait OptionInstances extends OptionInstances0 {
  implicit def catsKernelStdOrderForOption[A: Order]: Order[Option[A]] =
    new OptionOrder[A]
  implicit def catsKernelStdCommutativeMonoidForOption[A: CommutativeSemigroup]: CommutativeMonoid[Option[A]] =
    new OptionCommutativeMonoid[A]
  implicit def catsKernelStdMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    new OptionMonoid[A]
}

private[instances] trait OptionInstances0 extends OptionInstances1 {
  implicit def catsKernelStdPartialOrderForOption[A: PartialOrder]: PartialOrder[Option[A]] =
    new OptionPartialOrder[A]
}

private[instances] trait OptionInstances1 extends OptionInstances2 {
  implicit def catsKernelStdHashForOption[A: Hash]: Hash[Option[A]] =
    new OptionHash[A]
}

private[instances] trait OptionInstances2 {
  implicit def catsKernelStdEqForOption[A: Eq]: Eq[Option[A]] =
    new OptionEq[A]
}

class OptionOrder[A](implicit A: Order[A]) extends Order[Option[A]] {
  def compare(x: Option[A], y: Option[A]): Int =
    x match {
      case None =>
        if (y.isEmpty) 0 else -1
      case Some(a) =>
        y match {
          case None    => 1
          case Some(b) => A.compare(a, b)
        }
    }
}

class OptionPartialOrder[A](implicit A: PartialOrder[A]) extends PartialOrder[Option[A]] {
  def partialCompare(x: Option[A], y: Option[A]): Double =
    x match {
      case None =>
        if (y.isEmpty) 0.0 else -1.0
      case Some(a) =>
        y match {
          case None    => 1.0
          case Some(b) => A.partialCompare(a, b)
        }
    }
}

class OptionHash[A](implicit A: Hash[A]) extends OptionEq[A](A) with Hash[Option[A]] {
  def hash(x: Option[A]): Int =
    x match {
      case None     => None.hashCode()
      case Some(xx) => StaticMethods.product1HashWithPrefix(A.hash(xx), x.productPrefix)
    }
}

class OptionEq[A](implicit A: Eq[A]) extends Eq[Option[A]] {
  def eqv(x: Option[A], y: Option[A]): Boolean =
    x match {
      case None => y.isEmpty
      case Some(a) =>
        y match {
          case None    => false
          case Some(b) => A.eqv(a, b)
        }
    }
}

class OptionMonoid[A](implicit A: Semigroup[A]) extends Monoid[Option[A]] {
  def empty: Option[A] = None
  def combine(x: Option[A], y: Option[A]): Option[A] =
    x match {
      case None => y
      case Some(a) =>
        y match {
          case None    => x
          case Some(b) => Some(A.combine(a, b))
        }
    }
}

private class OptionCommutativeMonoid[A](implicit A: CommutativeSemigroup[A])
    extends OptionMonoid[A](A)
    with CommutativeMonoid[Option[A]]
