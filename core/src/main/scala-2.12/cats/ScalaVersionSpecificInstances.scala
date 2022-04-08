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

import cats.data.ZipStream

private[cats] trait ScalaVersionSpecificTraverseInstances {
  implicit def catsTraverseForStream: Traverse[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificShowInstances {
  implicit def catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]
}

private[cats] trait ScalaVersionSpecificSemigroupalInstances {
  implicit def catsSemigroupalForStream: Semigroupal[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificMonoidKInstances {
  implicit def catsMonoidKForStream: MonoidK[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificParallelInstances {
  implicit def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.stream.catsStdParallelForStreamZipStream
}

private[cats] trait ScalaVersionSpecificInvariantInstances {
  implicit def catsInstancesForStream: Monad[Stream] with Alternative[Stream] with CoflatMap[Stream] =
    cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificTraverseFilterInstances {
  implicit def catsTraverseFilterForStream: TraverseFilter[Stream] =
    cats.instances.stream.catsStdTraverseFilterForStream
}

private[cats] trait ScalaVersionSpecificAlignInstances {
  implicit def catsAlignForStream: Align[Stream] =
    cats.instances.stream.catsStdInstancesForStream
}
