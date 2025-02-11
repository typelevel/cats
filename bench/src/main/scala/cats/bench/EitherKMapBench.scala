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

package cats.bench

import cats.Functor
import cats.data.EitherK
import cats.syntax.all.*
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class EitherKMapBench {
  def map1[F[_], G[_], A, B](e: EitherK[F, G, A])(f: A => B)(implicit F: Functor[F], G: Functor[G]): EitherK[F, G, B] =
    EitherK(e.run.bimap(F.lift(f), G.lift(f)))

  def map2[F[_], G[_], A, B](e: EitherK[F, G, A])(f: A => B)(implicit F: Functor[F], G: Functor[G]): EitherK[F, G, B] =
    EitherK(
      e.run match {
        case Right(ga) => Right(G.map(ga)(f))
        case Left(fa)  => Left(F.map(fa)(f))
      }
    )

  type EK = EitherK[List, Option, Int]

  val value1 = EitherK[List, Option, Int](Right(Some(0)))
  val value2 = EitherK[List, Option, Int](Right(None))
  val value3 = EitherK[List, Option, Int](Left(List(1, 2)))
  val f: Int => Int = _ + 1

  @Benchmark
  def incMap1: (EK, EK, EK) = (map1(value1)(f), map1(value2)(f), map1(value3)(f))

  @Benchmark
  def incMap2: (EK, EK, EK) = (map2(value1)(f), map2(value2)(f), map2(value3)(f))
}
