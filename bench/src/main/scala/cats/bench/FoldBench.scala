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

import cats.data.Const
import cats.instances.string._
import cats.instances.list._
import cats.instances.vector._
import cats.instances.option._
import cats.{Foldable, Traverse}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FoldBench {

  val chars: List[String] = ('a' to 'z').map(_.toString).toList
  val charsVector: Vector[String] = chars.toVector
  val combineStringsSome: (String, String) => Option[String] = (s1, s2) => Some(s1 + s2)

  /** Benchmark fold of Foldable[List] */
  @Benchmark
  def fold(): String =
    Foldable[List].fold(chars)

  /** Benchmark fold using traverse with Const */
  @Benchmark
  def traverseConst(): String =
    Traverse[List].traverse[Const[String, *], String, String](chars)(Const(_)).getConst

  @Benchmark
  def vectorToListFoldM(): Option[String] =
    Foldable[List].foldM(charsVector.toList, "")(combineStringsSome)

  @Benchmark
  def vectorIndexFoldM(): Option[String] =
    Foldable[Vector].foldM(charsVector, "")(combineStringsSome)

}
