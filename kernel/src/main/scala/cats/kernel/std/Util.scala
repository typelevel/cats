package cats.kernel.std.util

import java.lang.Double.{ longBitsToDouble, doubleToLongBits }
import java.lang.Float.{ intBitsToFloat, floatToIntBits }
import java.lang.Long.{ numberOfTrailingZeros, numberOfLeadingZeros }
import java.lang.Math
import scala.annotation.tailrec
import scala.collection.mutable

object StaticMethods {

  /**
   * Implementation of the binary GCD algorithm.
   */
  final def gcd(x0: Long, y0: Long): Long = {
    // if either argument is 0, just return the other.
    if (x0 == 0L) return y0
    if (y0 == 0L) return x0

    val xz = numberOfTrailingZeros(x0)
    val yz = numberOfTrailingZeros(y0)

    // Math.abs is safe because Long.MinValue (0x8000000000000000)
    // will be shifted over 63 bits (due to trailing zeros).
    var x = Math.abs(x0 >> xz)
    var y = Math.abs(y0 >> yz)

    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }

    // trailing zeros mean powers of two -- if both numbers had
    // trailing zeros, those are part of the common divsor as well.
    if (xz < yz) x << xz else x << yz
  }

  /**
   * GCD for Float values.
   */
  final def gcd(a: Float, b: Float): Float = {
    import java.lang.Integer.{ numberOfTrailingZeros, numberOfLeadingZeros }

    def value(bits: Int): Int = bits & 0x007FFFFF | 0x00800000

    def exp(bits: Int): Int = ((bits >> 23) & 0xFF).toInt

    def gcd0(val0: Int, exp0: Int, val1: Int, exp1: Int): Float = {
      val tz0 = numberOfTrailingZeros(val0)
      val tz1 = numberOfTrailingZeros(val1)
      val tzShared = Math.min(tz0, tz1 + exp1 - exp0)
      val n = gcd(val0.toLong >>> tz0, val1.toLong >>> tz1).toInt << tzShared

      val shift = numberOfLeadingZeros(n) - 8 // Number of bits to move 1 to bit 23
      val mantissa = (n << shift) & 0x007FFFFF
      val exp = (exp0 - shift)
      if (exp < 0) 0F else intBitsToFloat((exp << 23) | mantissa)
    }

    if (a == 0F) b
    else if (b == 0F) a
    else {
      val aBits = floatToIntBits(a)
      val aVal = value(aBits)
      val aExp = exp(aBits)

      val bBits = floatToIntBits(b)
      val bVal = value(bBits)
      val bExp = exp(bBits)

      if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
      else gcd0(bVal, bExp, aVal, aExp)
    }
  }

  /**
   * GCD for Double values.
   */
  final def gcd(a: Double, b: Double): Double = {
    def value(bits: Long): Long = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L

    def exp(bits: Long): Int = ((bits >> 52) & 0x7FF).toInt

    def gcd0(val0: Long, exp0: Int, val1: Long, exp1: Int): Double = {
      val tz0 = numberOfTrailingZeros(val0)
      val tz1 = numberOfTrailingZeros(val1)
      val tzShared = Math.min(tz0, tz1 + exp1 - exp0)
      val n = gcd(val0 >>> tz0, val1 >>> tz1) << tzShared

      val shift = numberOfLeadingZeros(n) - 11 // Number of bits to move 1 to bit 52
      val mantissa = (n << shift) & 0x000FFFFFFFFFFFFFL
      val exp = (exp0 - shift).toLong
      if (exp < 0) 0.0 else longBitsToDouble((exp << 52) | mantissa)
    }

    if (a == 0D) b
    else if (b == 0D) a
    else {
      val aBits = doubleToLongBits(a)
      val aVal = value(aBits)
      val aExp = exp(aBits)

      val bBits = doubleToLongBits(b)
      val bVal = value(bBits)
      val bExp = exp(bBits)

      if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
      else gcd0(bVal, bExp, aVal, aExp)
    }
  }

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

    if (exponent >= 0L) loop(1L, base, exponent) else {
      if(base == 0L) throw new ArithmeticException("zero can't be raised to negative power")
      else if (base == 1L) 1L
      else if (base == -1L) if ((exponent & 1L) == 0L) -1L else 1L
      else 0L
    }
  }

  def initMutableMap[K, V](m: Map[K, V]): mutable.Map[K, V] = {
    val result = mutable.Map.empty[K, V]
    m.foreach { case (k, v) => result(k) = v }
    result
  }

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[kernel] class WrappedMutableMap[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
    def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
    def -(key: K): Map[K, V] = m.toMap - key
  }

  def addMap[K, V](x: Map[K, V], y: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    val (small, big, g) =
      if (x.size <= y.size) (x, y, f)
      else (y, x, (v1: V, v2: V) => f(v2, v1))

    val m = initMutableMap(big)
    small.foreach { case (k, v1) =>
      m(k) = m.get(k) match {
        case Some(v2) => g(v1, v2)
        case None => v1
      }
    }
    wrapMutableMap(m)
  }

  def subtractMap[K, V](x: Map[K, V], y: Map[K, V])(subtract: (V, V) => V)(negate: V => V): Map[K, V] = {
    // even if x is smaller, we'd need to call map/foreach on y to
    // negate all its values, so this is just as fast or faster.
    val m = initMutableMap(x)
    y.foreach { case (k, v2) =>
      m(k) = m.get(k) match {
        case Some(v1) => subtract(v1, v2)
        case None => negate(v2)
      }
    }
    wrapMutableMap(m)
  }
}
