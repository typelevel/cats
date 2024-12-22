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

class ZipLazyList[A](val value: LazyList[A]) extends AnyVal

object ZipLazyList {

  def apply[A](value: LazyList[A]): ZipLazyList[A] = new ZipLazyList(value)

  implicit val catsDataAlternativeForZipLazyList: Alternative[ZipLazyList] & CommutativeApplicative[ZipLazyList] =
    new Alternative[ZipLazyList] with CommutativeApplicative[ZipLazyList] {
      def pure[A](x: A): ZipLazyList[A] = new ZipLazyList(LazyList.continually(x))

      override def map[A, B](fa: ZipLazyList[A])(f: (A) => B): ZipLazyList[B] =
        ZipLazyList(fa.value.map(f))

      def ap[A, B](ff: ZipLazyList[A => B])(fa: ZipLazyList[A]): ZipLazyList[B] =
        ZipLazyList(ff.value.lazyZip(fa.value).map(_.apply(_)))

      override def product[A, B](fa: ZipLazyList[A], fb: ZipLazyList[B]): ZipLazyList[(A, B)] =
        ZipLazyList(fa.value.zip(fb.value))

      def empty[A]: ZipLazyList[A] = ZipLazyList(LazyList.empty[A])

      def combineK[A](x: ZipLazyList[A], y: ZipLazyList[A]): ZipLazyList[A] =
        ZipLazyList(cats.instances.lazyList.catsStdInstancesForLazyList.combineK(x.value, y.value))
    }

  implicit def catsDataEqForZipLazyList[A: Eq]: Eq[ZipLazyList[A]] =
    Eq.by((_: ZipLazyList[A]).value)(cats.kernel.instances.lazyList.catsKernelStdEqForLazyList[A])
}
