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

import scala.util.control.TailCalls.{done, tailcall, TailRec}

trait TailRecInstances {
  implicit def catsInstancesForTailRec: StackSafeMonad[TailRec] & Defer[TailRec] =
    TailRecInstances.catsInstancesForTailRec
}

private object TailRecInstances {
  val catsInstancesForTailRec: StackSafeMonad[TailRec] & Defer[TailRec] =
    new StackSafeMonad[TailRec] with Defer[TailRec] {
      def defer[A](fa: => TailRec[A]): TailRec[A] = tailcall(fa)

      def pure[A](a: A): TailRec[A] = done(a)

      override def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] =
        fa.map(f)

      def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
        fa.flatMap(f)

      override def unit: TailRec[Unit] = _unit
      private[this] val _unit: TailRec[Unit] = done(())
      override def void[A](ta: TailRec[A]): TailRec[Unit] = unit
    }
}
