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

import cats.kernel.CommutativeMonoid

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: UnorderedTraverse[Set] & MonoidK[Set] =
    new UnorderedTraverse[Set] with MonoidK[Set] {

      def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: Set[A])(f: A => G[B]): G[Set[B]] =
        sa.foldLeft(Applicative[G].pure(Set.empty[B])) { (acc, a) =>
          Apply[G].map2(acc, f(a))(_ + _)
        }

      override def unorderedSequence[G[_]: CommutativeApplicative, A](sa: Set[G[A]]): G[Set[A]] =
        sa.foldLeft(Applicative[G].pure(Set.empty[A])) { (acc, a) =>
          Apply[G].map2(acc, a)(_ + _)
        }

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def unorderedFoldMap[A, B](fa: Set[A])(f: A => B)(implicit B: CommutativeMonoid[B]): B =
        fa.foldLeft(B.empty)((b, a) => B.combine(f(a), b))

      override def unorderedFold[A](fa: Set[A])(implicit A: CommutativeMonoid[A]): A = A.combineAll(fa)

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty

    }

  implicit def catsStdShowForSet[A: Show]: Show[Set[A]] =
    _.iterator.map(Show[A].show).mkString("Set(", ", ", ")")
}
