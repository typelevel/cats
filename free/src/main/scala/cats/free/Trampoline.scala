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
package free

// To workaround SI-7139 `object Trampoline` needs to be defined inside the package object
// together with the type alias.
abstract private[free] class TrampolineFunctions {
  def done[A](a: A): Trampoline[A] =
    Free.pure[Function0, A](a)

  @deprecated("Use Trampoline.defer.", "1.0.0-MF")
  private[free] def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    defer(a)

  def defer[A](a: => Trampoline[A]): Trampoline[A] =
    Free.defer(a)

  def delay[A](a: => A): Trampoline[A] =
    defer(done(a))
}
