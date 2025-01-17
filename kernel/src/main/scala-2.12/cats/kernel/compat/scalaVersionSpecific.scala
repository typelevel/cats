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

package cats.kernel.compat
import scala.annotation.{Annotation, StaticAnnotation}
import scala.collection.{IterableLike, TraversableLike}

private[cats] object scalaVersionSpecific {

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionSpecific extends Annotation with StaticAnnotation

  type IterableOnce[+A] = TraversableOnce[A]

  implicit class traversableOnceExtension[A](private val to: TraversableOnce[A]) extends AnyVal {
    def iterator: Iterator[A] = to.toIterator

    def knownSize: Int = -1
  }

  implicit class doubleExtension(private val double: Double) extends AnyVal {
    def sign: Double = if (double.isNaN) Double.NaN else double.signum.toDouble
  }
  implicit class intExtension(private val i: Int) extends AnyVal {
    def sign: Int = i.signum
  }

  implicit class lazyZipExtension[A](private val a: A) extends AnyVal {
    def lazyZip[El1, Repr1, El2, Repr2, T](
      that: T
    )(implicit w1: A => TraversableLike[El1, Repr1], w2: T => IterableLike[El2, Repr2]) = (a, that).zipped
  }
}
