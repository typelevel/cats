package cats.kernel

import scala.annotation.implicitNotFound

/**
 * Lifting of the [[Hash]] class to binary type constructors.
 */
@implicitNotFound("Could not find an instance of Hash2 for ${F}")
trait Hash2[F[_, _]] extends Any with Eq2[F] {

  /**
   * Lift hashing through the type constructor.
   */
  def liftHash2[A, B](hashA: A => Int, hashB: B => Int, x: F[A, B]): Int

  // derived

  def hash2[A, B](x: F[A, B])(implicit A: Hash[A], B: Hash[B]): Int =
    liftHash2(A.hash, B.hash, x)
}

object Hash2 extends Hash2Instances0 {
  @inline def apply[F[_, _]](implicit ev: Hash2[F]): Hash2[F] = ev
}

private[kernel] trait Hash2Instances0 {
  implicit def catsKernelHash2ForEither: Hash2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
