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
package laws

import cats.kernel.CommutativeMonoid

trait UnorderedFoldableLaws[F[_]] {
  implicit def F: UnorderedFoldable[F]

  def unorderedFoldConsistentWithUnorderedFoldMap[A: CommutativeMonoid](fa: F[A]): IsEq[A] =
    F.unorderedFoldMap(fa)(identity) <-> F.unorderedFold(fa)

  def unorderedFoldMapAIdentity[A, B: CommutativeMonoid](fa: F[A], f: A => B): IsEq[B] =
    F.unorderedFoldMapA[Id, A, B](fa)(f) <-> F.unorderedFoldMap(fa)(f)

  def forallConsistentWithExists[A](fa: F[A], p: A => Boolean): Boolean =
    if (F.forall(fa)(p)) {
      val negationExists = F.exists(fa)(a => !p(a))

      // if p is true for all elements, then there cannot be an element for which
      // it does not hold.
      !negationExists &&
      // if p is true for all elements, then either there must be no elements
      // or there must exist an element for which it is true.
      (F.isEmpty(fa) || F.exists(fa)(p))
    } else true // can't test much in this case

  def existsLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.exists(fa) { _ =>
      i += 1
      true
    }
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def forallLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.forall(fa) { _ =>
      i += 1
      false
    }
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  /**
   * If `F[A]` is empty, forall must return true.
   */
  def forallEmpty[A](fa: F[A], p: A => Boolean): Boolean =
    !F.isEmpty(fa) || F.forall(fa)(p)

  def nonEmptyRef[A](fa: F[A]): IsEq[Boolean] =
    F.nonEmpty(fa) <-> !F.isEmpty(fa)

  def containsConsistentWithExists[A](fa: F[A], v: A)(implicit eq: Eq[A]): IsEq[Boolean] =
    F.contains_(fa, v) <-> F.exists(fa)(a => eq.eqv(a, v))

  def containsConsistentWithForall[A](fa: F[A], v: A)(implicit eq: Eq[A]): IsEq[Boolean] =
    !F.contains_(fa, v) <-> F.forall(fa)(a => eq.neqv(a, v))

  def containsAllElementsFromItself[A](fa: F[A])(implicit eq: Eq[A]): Boolean =
    F.forall(fa)(a => F.contains_(fa, a))
}

object UnorderedFoldableLaws {
  def apply[F[_]](implicit ev: UnorderedFoldable[F]): UnorderedFoldableLaws[F] =
    new UnorderedFoldableLaws[F] { def F: UnorderedFoldable[F] = ev }
}
