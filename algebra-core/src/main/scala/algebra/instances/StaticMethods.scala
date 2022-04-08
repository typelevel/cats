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

package algebra.instances

import scala.annotation.tailrec

object StaticMethods {

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^ex doesn't fit in a Long, the result will overflow (unlike
   * Math.pow which will return +/- Infinity).
   */
  final def pow(base: Long, exponent: Long): Long = {
    @tailrec def loop(t: Long, b: Long, e: Long): Long =
      if (e == 0L) t
      else if ((e & 1) == 1) loop(t * b, b * b, e >>> 1L)
      else loop(t, b * b, e >>> 1L)

    if (exponent >= 0L) loop(1L, base, exponent)
    else {
      if (base == 0L) throw new ArithmeticException("zero can't be raised to negative power")
      else if (base == 1L) 1L
      else if (base == -1L) if ((exponent & 1L) == 0L) -1L else 1L
      else 0L
    }
  }
}
