package algebra
package ring

import scala.{specialized => sp}
import scala.annotation.tailrec

/**
 * Ring consists of:
 *
 *  - a commutative group for addition (+)
 *  - a monoid for multiplication (*)
 *
 * Additionally, multiplication must distribute over addition.
 *
 * Ring implements some methods (for example fromInt) in terms of
 * other more fundamental methods (zero, one and plus). Where
 * possible, these methods should be overridden by more efficient
 * implementations.
 */
trait Ring[@sp(Int, Long, Float, Double) A] extends Any with Rig[A] with Rng[A] {

  /**
   * Convert the given integer to an instance of A.
   *
   * Defined to be equivalent to `sumN(one, n)`.
   *
   * That is, `n` repeated summations of this ring's `one`, or `-n`
   * summations of `-one` if `n` is negative.
   *
   * Most type class instances should consider overriding this method
   * for performance reasons.
   */
  def fromInt(n: Int): A = sumN(one, n)

  /**
   * Convert the given BigInt to an instance of A.
   *
   * This is equivalent to `n` repeated summations of this ring's `one`, or
   * `-n` summations of `-one` if `n` is negative.
   *
   * Most type class instances should consider overriding this method for
   * performance reasons.
   */
  def fromBigInt(n: BigInt): A = Ring.defaultFromBigInt(n)(this)
}

trait RingFunctions[R[T] <: Ring[T]] extends AdditiveGroupFunctions[R] with MultiplicativeMonoidFunctions[R] {
  def fromInt[@sp(Int, Long, Float, Double) A](n: Int)(implicit ev: R[A]): A =
    ev.fromInt(n)

  def fromBigInt[@sp(Int, Long, Float, Double) A](n: BigInt)(implicit ev: R[A]): A =
    ev.fromBigInt(n)

  final def defaultFromBigInt[@sp(Int, Long, Float, Double) A](n: BigInt)(implicit ev: R[A]): A = {
    if (n.isValidInt) {
      ev.fromInt(n.toInt)
    } else {
      val d = ev.fromInt(1 << 30)
      val mask = (1L << 30) - 1
      @tailrec def loop(k: A, x: BigInt, acc: A): A =
        if (x.isValidInt) {
          ev.plus(ev.times(k, ev.fromInt(x.toInt)), acc)
        } else {
          val y = x >> 30
          val r = ev.fromInt((x & mask).toInt)
          loop(ev.times(d, k), y, ev.plus(ev.times(k, r), acc))
        }

      val absValue = loop(one, n.abs, zero)
      if (n.signum < 0) ev.negate(absValue) else absValue
    }
  }

  /**
   * Returns the given Double, understood as a rational number, in the provided
   * (division) ring.
   *
   * This is implemented in terms of basic ops. However, this is
   * probably significantly less efficient than can be done with a specific
   * type. So, it is recommended to specialize this general method.
   */
  final def defaultFromDouble[A](a: Double)(implicit ringA: Ring[A], mgA: MultiplicativeGroup[A]): A =
    if (a == 0.0) ringA.zero
    else if (a.isValidInt) ringA.fromInt(a.toInt)
    else {
      import java.lang.Double.{doubleToLongBits, isInfinite, isNaN}
      import java.lang.Long.numberOfTrailingZeros
      require(!isInfinite(a) && !isNaN(a), "Double must be representable as a fraction.")
      val bits = doubleToLongBits(a)
      val expBits = ((bits >> 52) & 0x7ff).toInt
      val mBits = bits & 0x000fffffffffffffL
      // If expBits is 0, then this is a subnormal and we drop the implicit
      // 1 bit.
      val m = if (expBits > 0) mBits | 0x0010000000000000L else mBits
      val zeros = numberOfTrailingZeros(m)
      val value = m >>> zeros
      // If expBits is 0, then this is a subnormal with expBits = 1.
      val exp = math.max(1, expBits) - 1075 + zeros // 1023 + 52

      val high = ringA.times(ringA.fromInt((value >>> 30).toInt), ringA.fromInt(1 << 30))
      val low = ringA.fromInt((value & 0x3fffffff).toInt)
      val num = ringA.plus(high, low)
      val unsigned = if (exp > 0) {
        ringA.times(num, ringA.pow(ringA.fromInt(2), exp))
      } else if (exp < 0) {
        mgA.div(num, ringA.pow(ringA.fromInt(2), -exp))
      } else {
        num
      }

      if (a < 0) ringA.negate(unsigned) else unsigned
    }

}

object Ring extends RingFunctions[Ring] {
  @inline final def apply[A](implicit ev: Ring[A]): Ring[A] = ev
}
