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

import cats.data._
import cats.kernel.Semigroup
import cats.{NonEmptyParallel, Parallel}

trait ParallelInstances extends ParallelInstances1 {

  @deprecated("Use cats.instances.either.catsParallelForEitherAndValidated", "2.1.0")
  def catsParallelForEitherValidated[E: Semigroup]: Parallel.Aux[Either[E, *], Validated[E, *]] =
    cats.instances.either.catsParallelForEitherAndValidated[E]

  @deprecated("Use OptionT.catsDataParallelForOptionT", "2.7.0")
  def catsParallelForOptionTNestedOption[M[_]](implicit
    P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] = OptionT.catsDataParallelForOptionT[M]

  @deprecated("Use cats.instances.list.catsStdNonEmptyParallelForListZipList", "2.1.0")
  def catsStdNonEmptyParallelForZipList: NonEmptyParallel.Aux[List, ZipList] =
    cats.instances.list.catsStdNonEmptyParallelForListZipList

  @deprecated("Use cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector", "2.1.0")
  def catsStdNonEmptyParallelForZipVector: NonEmptyParallel.Aux[Vector, ZipVector] =
    cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector

  @deprecated("Use cats.instances.stream.catsStdParallelForStreamZipStream", "2.1.0")
  def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.stream.catsStdParallelForStreamZipStream

  @deprecated("Use EitherT.catsDataParallelForEitherTWithParallelEffect", "2.7.0")
  def catsParallelForEitherTNestedParallelValidated[M[_], E: Semigroup](implicit
    P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    EitherT.catsDataParallelForEitherTWithParallelEffect[M, E]
}
