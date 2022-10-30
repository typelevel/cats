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

import cats.platform.Platform

/**
 * Laws that must be obeyed by any `Defer`.
 */
trait DeferLaws[F[_]] {
  implicit def F: Defer[F]

  def deferIdentity[A](fa: Unit => F[A]): IsEq[F[A]] =
    F.defer(fa(())) <-> fa(())

  def deferDoesNotEvaluate[A](fa: Unit => F[A]): IsEq[Boolean] = {
    var evaluated = false
    val _ = F.defer {
      evaluated = true;
      fa(())
    }
    evaluated <-> false
  }

  def deferIsStackSafe[A](fa: Unit => F[A]): IsEq[F[A]] = {
    def loop(c: Int): F[A] =
      if (c <= 0) F.defer(fa(()))
      else F.defer(loop(c - 1))

    val cnt = if (Platform.isJvm) 20000 else 2000
    loop(cnt) <-> (fa(()))
  }

  def deferMatchesFix[A](fa: Unit => F[A]): IsEq[F[A]] = {
    val defered = F.defer(fa(()))
    val viaFix = F.fix[A](_ => fa(()))
    defered <-> viaFix
  }
}

object DeferLaws {
  def apply[F[_]](implicit ev: Defer[F]): DeferLaws[F] =
    new DeferLaws[F] { def F: Defer[F] = ev }
}
