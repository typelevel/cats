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

package cats.kernel

private[kernel] trait ScalaVersionSpecificOrderInstances extends ScalaVersionSpecificPartialOrderInstances {
  implicit def catsKernelOrderForStream[A: Order]: Order[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdOrderForStream[A]
}

private[kernel] trait ScalaVersionSpecificPartialOrderInstances extends ScalaVersionSpecificHashInstances {
  implicit def catsKernelPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdPartialOrderForStream[A]
}

private[kernel] trait ScalaVersionSpecificHashInstances extends ScalaVersionSpecificEqInstances {
  implicit def catsKernelHashForStream[A: Hash]: Hash[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdHashForStream[A]
}

private[kernel] trait ScalaVersionSpecificEqInstances {
  implicit def catsKernelEqForStream[A: Eq]: Eq[Stream[A]] = cats.kernel.instances.stream.catsKernelStdEqForStream[A]
}

private[kernel] trait ScalaVersionSpecificMonoidInstances {
  implicit def catsKernelMonoidForStream[A]: Monoid[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdMonoidForStream[A]
}
