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

import scala.collection.immutable.Seq
import kernel.compat.scalaVersionSpecific.*

class ZipSeq[A](val value: Seq[A]) extends AnyVal

@suppressUnusedImportWarningForScalaVersionSpecific
object ZipSeq {

  def apply[A](value: Seq[A]): ZipSeq[A] = new ZipSeq(value)

  implicit val catsDataCommutativeApplyForZipSeq: CommutativeApply[ZipSeq] = new CommutativeApply[ZipSeq] {

    override def map[A, B](fa: ZipSeq[A])(f: (A) => B): ZipSeq[B] =
      ZipSeq(fa.value.map(f))
    def ap[A, B](ff: ZipSeq[A => B])(fa: ZipSeq[A]): ZipSeq[B] =
      ZipSeq(ff.value.lazyZip(fa.value).map(_.apply(_)))

  }

  implicit def catsDataEqForZipSeq[A: Eq]: Eq[ZipSeq[A]] = Eq.by(_.value)
}
