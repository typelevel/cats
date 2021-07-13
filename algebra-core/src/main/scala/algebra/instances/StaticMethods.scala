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
