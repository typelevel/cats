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

package cats.instances

import cats.Functor

import scala.collection.mutable.Builder

private[cats] object StaticMethods {

  def appendAll[F <: Iterable[A], A](it: Iterator[F], bldr: Builder[A, F]): bldr.type = {
    while (it.hasNext) {
      bldr ++= it.next()
    }
    bldr
  }

  def mapAccumulateFromStrictFunctor[S, F[_], A, B](init: S, fa: F[A], f: (S, A) => (S, B))(implicit
    ev: Functor[F]
  ): (S, F[B]) = {
    var state = init

    val fb = ev.map(fa) { a =>
      val (newState, b) = f(state, a)
      state = newState
      b
    }

    (state, fb)
  }

  def mapWithIndexFromStrictFunctor[F[_], A, B](fa: F[A], f: (A, Int) => B)(implicit ev: Functor[F]): F[B] = {
    var idx = 0

    ev.map(fa) { a =>
      val b = f(a, idx)

      idx += 1

      b
    }
  }

  def mapWithLongIndexFromStrictFunctor[F[_], A, B](fa: F[A], f: (A, Long) => B)(implicit ev: Functor[F]): F[B] = {
    var idx: Long = 0L

    ev.map(fa) { a =>
      val b = f(a, idx)
      idx += 1
      b
    }
  }

}
