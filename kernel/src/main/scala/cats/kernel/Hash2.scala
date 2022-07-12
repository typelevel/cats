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

package cats.kernel

/**
 * Lifting of the [[Hash]] class to binary type constructors.
 */
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
