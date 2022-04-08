/*
 * Copyright (c) 2022 Typelevel
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
package data

import scala.annotation.tailrec
import scala.collection.mutable.Builder

abstract private[data] class OneAndLowPriority4 {
  implicit val catsDataComonadForNonEmptyStream: Comonad[OneAnd[Stream, *]] =
    new Comonad[OneAnd[Stream, *]] {
      def coflatMap[A, B](fa: OneAnd[Stream, A])(f: OneAnd[Stream, A] => B): OneAnd[Stream, B] = {
        @tailrec def consume(as: Stream[A], buf: Builder[B, Stream[B]]): Stream[B] =
          if (as.isEmpty) buf.result()
          else {
            val tail = as.tail
            consume(tail, buf += f(OneAnd(as.head, tail)))
          }
        OneAnd(f(fa), consume(fa.tail, Stream.newBuilder))
      }

      def extract[A](fa: OneAnd[Stream, A]): A =
        fa.head

      def map[A, B](fa: OneAnd[Stream, A])(f: A => B): OneAnd[Stream, B] =
        fa.map(f)(cats.instances.stream.catsStdInstancesForStream)
    }
}
