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

import kernel.compat.scalaVersionSpecific.*

class ZipList[A](val value: List[A]) extends AnyVal

@suppressUnusedImportWarningForScalaVersionSpecific
object ZipList {

  def apply[A](value: List[A]): ZipList[A] = new ZipList(value)

  implicit val catsDataCommutativeApplyForZipList: CommutativeApply[ZipList] = new CommutativeApply[ZipList] {

    override def map[A, B](fa: ZipList[A])(f: (A) => B): ZipList[B] =
      ZipList(fa.value.map(f))

    def ap[A, B](ff: ZipList[A => B])(fa: ZipList[A]): ZipList[B] =
      ZipList(ff.value.lazyZip(fa.value).map(_.apply(_)))

    override def product[A, B](fa: ZipList[A], fb: ZipList[B]): ZipList[(A, B)] =
      ZipList(fa.value.zip(fb.value))

  }

  implicit def catsDataEqForZipList[A: Eq]: Eq[ZipList[A]] = Eq.by(_.value)
}
