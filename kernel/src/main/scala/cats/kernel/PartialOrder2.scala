package cats.kernel

/**
 * Lifting of the [[PartialOrder]] class to binary type constructors.
 */
trait PartialOrder2[F[_, _]] extends Any with Eq2[F] {

  /**
   * Lift partial ordering tests through the type constructor.
   */
  def liftPartialCompare2[A, B, C, D](partialCompareAB: (A, B) => Double,
                                      partialCompareCD: (C, D) => Double,
                                      x: F[A, C],
                                      y: F[B, D]
  ): Double

  // derived //

  // from Eq2

  // In order to implement liftEq2 in terms of liftPartialCompare2, we need to
  // choose Double values to represent true and false. We choose 0 to
  // represent true, to match with the canonical definition of PartialOrder
  // equality, and Double.NaN to match false.
  override def liftEq2[A, B, C, D](compareAB: (A, B) => Boolean,
                                   compareCD: (C, D) => Boolean,
                                   x: F[A, C],
                                   y: F[B, D]
  ): Boolean =
    liftPartialCompare2[A, B, C, D](
      (a, b) => if (compareAB(a, b)) 0d else Double.NaN,
      (c, d) => if (compareCD(c, d)) 0d else Double.NaN,
      x,
      y
    ) == 0d

  // other //

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   *
   *   - negative iff `x < y`
   *   - zero     iff `x = y`
   *   - positive iff `x > y`
   */
  def partialCompare2[A, B](x: F[A, B], y: F[A, B])(implicit A: PartialOrder[A], B: PartialOrder[B]): Double =
    liftPartialCompare2[A, A, B, B](A.partialCompare, B.partialCompare, x, y)
}

object PartialOrder2 extends PartialOrder2Instances0 {
  @inline def apply[F[_, _]](implicit ev: PartialOrder2[F]): PartialOrder2[F] =
    ev
}

private[kernel] trait PartialOrder2Instances0 {
  implicit def catsKernelPartialOrder2ForEither: PartialOrder2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
