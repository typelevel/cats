package cats.kernel

import java.lang.Double.isNaN

/**
 * Lifting of the [[Order]] class to binary type constructors.
 *
 * @note The default implementation of the [[PartialOrder2]] provided in this
 *       class is lossy in terms of magnitude. That is, if the underlying
 *       comparison returns `-100.123`, it will be converted into `-1.0`. If
 *       for some reason you desire the magnitude to be preserved for the
 *       [[PartialOrder2]] instance, then you should override this function.
 *
 * @see [[https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor-Classes.html#t:Ord2]]
 */
trait Order2[F[_, _]] extends Any with PartialOrder2[F] {

  /**
   * Lift ordering tests through the type constructor.
   */
  def liftCompare2[A, B, C, D](compareAB: (A, B) => Int, compareCD: (C, D) => Int, x: F[A, C], y: F[B, D]): Int

  // derived

  // from PartialOrder2

  override def liftPartialCompare2[A, B, C, D](partialCompareAB: (A, B) => Double,
                                               partialCompareCD: (C, D) => Double,
                                               x: F[A, C],
                                               y: F[B, D]
  ): Double = {
    // If we desire to define liftPartialCompare2 in terms of liftCompare2 we
    // have to deal with a few issues. Double is a 64-bit value, but Int is a
    // 32-bit value. To define liftPartialCompare2 in terms of liftCompare2 we
    // need to use the Int value to hold the intermediate value of our
    // partialCompareAB or partialCompareCD functions. Because Int only has
    // 32-bits to work with, our implementation _must_ be lossy in terms of
    // the magnitude of the result. For all implementations of PartialOrder2,
    // this is valid, as the magnitude is not considered in the semantics of
    // the class.
    //
    // Additionally, we need to pick a sentinel Int value to represent the
    // Double.NaN result. This is because a type with an Order2 instance,
    // might be used to generate a PartialOrder instance for some type F[A, B]
    // for which A and B have a PartialOrder but not Order. In this
    // implementation we chose Int.MinValue as our Double.NaN sentinel value,
    // though it could be any Int value that is not -1, 0, or 1.

    val nanInt: Int = Int.MinValue

    // Convert a Double result into an Int result to use with the liftCompare2
    // function. This function is lossy in terms of the magnitude of the
    // result.
    def partialToTotal(value: Double): Int =
      if (isNaN(value)) {
        nanInt
      } else if (value < 0d) {
        -1
      } else if (value > 0d) {
        1
      } else {
        0
      }

    // Convert an Int result as defined by partialToTotal into a Double
    // result, checking for the sentinel value and returning Double.NaN when
    // that is present.
    def totalToPartial(value: Int): Double =
      if (value == nanInt) {
        Double.NaN
      } else {
        value.toDouble
      }

    totalToPartial(
      liftCompare2(
        ((a: A, b: B) => partialToTotal(partialCompareAB(a, b))),
        ((c: C, d: D) => partialToTotal(partialCompareCD(c, d))),
        x,
        y
      )
    )
  }
}

object Order2 extends Order2Instances0 {
  @inline def apply[F[_, _]](implicit ev: Order2[F]): Order2[F] =
    ev
}

private[kernel] trait Order2Instances0 {
  implicit def catsKernelOrder2ForEither: Order2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
