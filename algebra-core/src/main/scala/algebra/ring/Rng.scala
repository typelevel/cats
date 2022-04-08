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

package algebra
package ring

import scala.{specialized => sp}

/**
 * Rng (pronounced "Rung") consists of:
 *
 *  - a commutative group for addition (+)
 *  - a semigroup for multiplication (*)
 *
 * Alternately, a Rng can be thought of as a ring without a
 * multiplicative identity (or as a semiring with an additive
 * inverse).
 *
 * Mnemonic: "Rng is a Ring without multiplicative 'I'dentity."
 */
trait Rng[@sp(Int, Long, Float, Double) A] extends Any with Semiring[A] with AdditiveCommutativeGroup[A]

object Rng extends AdditiveGroupFunctions[Rng] with MultiplicativeSemigroupFunctions[Rng] {
  @inline final def apply[A](implicit ev: Rng[A]): Rng[A] = ev
}
