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

/**
 * An instance of `NotNull[A]` indicates that `A` does not have a static type
 * of `Null`.
 *
 * This can be useful in preventing `Null` from being inferred when a type
 * parameter is omitted.
 *
 * This trait is used along with ambiguous implicits to achieve the goal of
 * preventing inference of `Null`. This ambiguous implicit trick has been used
 * in the Scala community for some time. [[https://gist.github.com/milessabin/de58f3ba7024d51dcc1a Here]]
 * is an early example of such a trick being used in a similar way to prevent a
 * `Nothing` type.
 */
sealed trait NotNull[A]

object NotNull {

  /**
   * Since NotNull is just a marker trait with no functionality, it's safe to
   * reuse a single instance of it. This helps prevent unnecessary allocations.
   */
  private[this] val singleton: NotNull[Any] = new NotNull[Any] {}

  private[this] def ambiguousException: Exception =
    new Exception(
      "An instance of NotNull[Null] was used. This should never happen. Both ambiguous NotNull[Null] instances should always be in scope if one of them is."
    )

  implicit def `If you are seeing this, you probably need to add an explicit type parameter somewhere, because Null is being inferred.`
    : NotNull[Null] = throw ambiguousException

  implicit def catsAmbiguousNotNullNull2: NotNull[Null] = throw ambiguousException

  implicit def catsNotNullForA[A]: NotNull[A] = singleton.asInstanceOf[NotNull[A]]
}
