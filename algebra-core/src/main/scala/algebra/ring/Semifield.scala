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
 * Semifield consists of:
 *
 *  - a commutative monoid for addition (+)
 *  - a group for multiplication (*)
 *
 * Alternately, a Semifield can be thought of as a DivisionRing without an additive inverse.
 */
trait Semifield[@sp(Int, Long, Float, Double) A] extends Any with Rig[A] with MultiplicativeGroup[A]

object Semifield extends AdditiveMonoidFunctions[Semifield] with MultiplicativeGroupFunctions[Semifield] {
  @inline final def apply[A](implicit ev: Semifield[A]): Semifield[A] = ev
}
