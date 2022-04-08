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

package alleycats
package std

import cats.Bimonad

import scala.util.Try

object try_ extends TryInstances

trait TryInstances {
  // There are various concerns people have over Try's ability to
  // satisfy the monad laws. For example, consider the following code:
  //
  //     import scala.util.{Try, Success}
  //
  //     def verify(n: Int): Try[Int] =
  //       if (n == 0) sys.error("nope") else Success(n)
  //
  //     val x = Try(0).flatMap(verify)
  //     val y = verify(0)
  //
  // The monad laws require that `x` and `y` produce the same value,
  // but in this case `x` is a `Failure(_)` and `y` is undefined (due
  // an error being thrown).
  //
  // Since `verify` is not a total function, it is arguable whether
  // this constitutes a law violation, but there is enough concern
  // that the Monad[Try] instance has ended up here in Alleycats.
  //
  // Furthermore, since Cats has introduced a Bimonad[A], the Monad[Try]
  // and Comonad[Try] instances have been replaced by a single Bimonad[Try]
  // instance.
  //
  implicit val alleycatsStdTryBimonad: Bimonad[Try] =
    new Bimonad[Try] {
      def pure[A](a: A): Try[A] = Try(a)
      override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
      def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
      def coflatMap[A, B](fa: Try[A])(f: Try[A] => B): Try[B] = Try(f(fa))
      def extract[A](p: Try[A]): A = p.get

      def tailRecM[A, B](a: A)(f: (A) => Try[Either[A, B]]): Try[B] =
        cats.instances.try_.catsStdInstancesForTry.tailRecM(a)(f)
    }
}
