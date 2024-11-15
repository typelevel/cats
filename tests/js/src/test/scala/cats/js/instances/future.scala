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
package js
package instances

import scala.concurrent.Future
import scala.concurrent.{ExecutionContext => E}
import scala.concurrent.duration.FiniteDuration

import cats.instances.FutureCoflatMap
import cats.syntax.all.*

object future extends FutureInstances0

object Await {
  def result[A](f: Future[A], atMost: FiniteDuration): A =
    f.value match {
      case Some(v) => v.get
      case None    => throw new IllegalStateException()
    }
}

sealed private[instances] trait FutureInstances0 extends FutureInstances1 {
  def futureComonad(atMost: FiniteDuration)(implicit ec: E): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {
      def extract[A](x: Future[A]): A =
        Await.result(x, atMost)
    }

  def futureOrder[A: Order](atMost: FiniteDuration)(implicit ec: E): Order[Future[A]] =
    (x, y) => Await.result(x.zip(y).map { case (x, y) => x.compare(y) }, atMost)
}

sealed private[instances] trait FutureInstances1 extends FutureInstances2 {
  def futurePartialOrder[A: PartialOrder](atMost: FiniteDuration)(implicit ec: E): PartialOrder[Future[A]] =
    (x, y) => Await.result(x.zip(y).map { case (x, y) => x.partialCompare(y) }, atMost)
}

sealed private[instances] trait FutureInstances2 {
  def futureEq[A: Eq](atMost: FiniteDuration)(implicit ec: E): Eq[Future[A]] =
    (x, y) => Await.result(x.zip(y).map { case (x, y) => x === y }, atMost)
}
