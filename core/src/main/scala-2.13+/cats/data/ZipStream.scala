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
package data

@deprecated("Use ZipLazyList", "2.0.0-RC2")
class ZipStream[A](val value: Stream[A]) extends AnyVal

@deprecated("Use ZipLazyList", "2.0.0-RC2")
object ZipStream {

  def apply[A](value: Stream[A]): ZipStream[A] = new ZipStream(value)

  implicit val catsDataAlternativeForZipStream: Alternative[ZipStream] with CommutativeApplicative[ZipStream] =
    new Alternative[ZipStream] with CommutativeApplicative[ZipStream] {
      def pure[A](x: A): ZipStream[A] = new ZipStream(Stream.continually(x))

      override def map[A, B](fa: ZipStream[A])(f: (A) => B): ZipStream[B] =
        ZipStream(fa.value.map(f))

      def ap[A, B](ff: ZipStream[A => B])(fa: ZipStream[A]): ZipStream[B] =
        ZipStream(ff.value.lazyZip(fa.value).map(_.apply(_)))

      override def product[A, B](fa: ZipStream[A], fb: ZipStream[B]): ZipStream[(A, B)] =
        ZipStream(fa.value.zip(fb.value))

      def empty[A]: ZipStream[A] = ZipStream(Stream.empty[A])

      def combineK[A](x: ZipStream[A], y: ZipStream[A]): ZipStream[A] =
        ZipStream(cats.instances.stream.catsStdInstancesForStream.combineK(x.value, y.value))
    }

  implicit def catsDataEqForZipStream[A: Eq]: Eq[ZipStream[A]] =
    Eq.by((_: ZipStream[A]).value)(cats.kernel.instances.stream.catsKernelStdEqForStream[A])
}
