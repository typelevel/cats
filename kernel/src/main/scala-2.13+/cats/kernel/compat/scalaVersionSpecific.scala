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
package kernel

package compat
import scala.annotation.{Annotation, StaticAnnotation}
import scala.jdk.javaapi.CollectionConverters

private[cats] object scalaVersionSpecific {

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionSpecific extends Annotation with StaticAnnotation

  implicit class iterableOnceExtension[A](private val io: IterableOnce[A]) extends AnyVal {
    def reduceOption(f: (A, A) => A): Option[A] = io.iterator.reduceOption(f)
  }

  implicit class setExtension[A](private val s: java.util.Set[A]) extends AnyVal {
    def asScala: Iterable[A] = CollectionConverters.asScala(s)
  }
}
