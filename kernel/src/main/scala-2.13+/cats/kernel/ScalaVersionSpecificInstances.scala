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

package cats.kernel

import scala.collection.immutable.ArraySeq

private[kernel] trait ScalaVersionSpecificOrderInstances extends ScalaVersionSpecificPartialOrderInstances {
  @deprecated("Use catsKernelOrderForLazyList", "3.0.0")
  implicit def catsKernelOrderForStream[A: Order]: Order[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdOrderForStream[A]

  implicit def catsKernelOrderForLazyList[A: Order]: Order[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdOrderForLazyList[A]

  implicit def catsKernelOrderForArraySeq[A: Order]: Order[ArraySeq[A]] =
    cats.kernel.instances.arraySeq.catsKernelStdOrderForArraySeq[A]
}

private[kernel] trait ScalaVersionSpecificPartialOrderInstances extends ScalaVersionSpecificHashInstances {
  @deprecated("Use catsKernelPartialOrderForLazyList", "3.0.0")
  implicit def catsKernelPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdPartialOrderForStream[A]

  implicit def catsKernelPartialOrderForLazyList[A: PartialOrder]: PartialOrder[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdPartialOrderForLazyList[A]

  implicit def catsKernelPartialOrderForArraySeq[A: PartialOrder]: PartialOrder[ArraySeq[A]] =
    cats.kernel.instances.arraySeq.catsKernelStdPartialOrderForArraySeq[A]
}

private[kernel] trait ScalaVersionSpecificHashInstances extends ScalaVersionSpecificEqInstances {
  @deprecated("Use catsKernelHashForLazyList", "3.0.0")
  implicit def catsKernelHashForStream[A: Hash]: Hash[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdHashForStream[A]

  implicit def catsKernelHashForLazyList[A: Hash]: Hash[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdHashForLazyList[A]

  implicit def catsKernelHashForArraySeq[A: Hash]: Hash[ArraySeq[A]] =
    cats.kernel.instances.arraySeq.catsKernelStdHashForArraySeq[A]
}

private[kernel] trait ScalaVersionSpecificEqInstances {
  @deprecated("Use catsKernelEqForLazyList", "3.0.0")
  implicit def catsKernelEqForStream[A: Eq]: Eq[Stream[A]] = cats.kernel.instances.stream.catsKernelStdEqForStream[A]

  implicit def catsKernelEqForLazyList[A: Eq]: Eq[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdEqForLazyList[A]

  implicit def catsKernelEqForArraySeq[A: Eq]: Eq[ArraySeq[A]] =
    cats.kernel.instances.arraySeq.catsKernelStdEqForArraySeq[A]
}

private[kernel] trait ScalaVersionSpecificMonoidInstances {
  @deprecated("Use catsKernelMonoidForLazyList", "3.0.0")
  implicit def catsKernelMonoidForStream[A]: Monoid[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdMonoidForStream[A]

  implicit def catsKernelMonoidForLazyList[A]: Monoid[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdMonoidForLazyList[A]

  implicit def catsKernelMonoidForArraySeq[A]: Monoid[ArraySeq[A]] =
    cats.kernel.instances.arraySeq.catsKernelStdMonoidForArraySeq[A]
}
