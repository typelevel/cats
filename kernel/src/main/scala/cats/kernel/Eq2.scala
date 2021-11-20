package cats.kernel

import scala.annotation.implicitNotFound

/**
 * Lifting of the [[Eq]] class to binary type constructors.
 *
 * @see [[https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor-Classes.html#t:Eq2]]
 */
@implicitNotFound("Could not find an instance of Eq2 for ${F}")
trait Eq2[F[_, _]] extends Any with Serializable {

  /**
   * Lift equality tests through the type constructor.
   */
  def liftEq2[A, B, C, D](compareAB: (A, B) => Boolean, compareCD: (C, D) => Boolean, x: F[A, C], y: F[B, D]): Boolean

  // derived //

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv2[A, B](x: F[A, B], y: F[A, B])(implicit A: Eq[A], B: Eq[B]): Boolean =
    liftEq2[A, A, B, B](A.eqv, B.eqv, x, y)

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv2[A: Eq, B: Eq](x: F[A, B], y: F[A, B]): Boolean =
    !eqv2[A, B](x, y)
}

object Eq2 extends Eq2Instances0 {
  @inline def apply[F[_, _]](implicit ev: Eq2[F]): Eq2[F] = ev
}

private[kernel] trait Eq2Instances0 {
  implicit def catsKernelEq2ForEither: Eq2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
