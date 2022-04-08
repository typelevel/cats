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
package lattice

import scala.{specialized => sp}

/**
 * Heyting algebras are bounded lattices that are also equipped with
 * an additional binary operation `imp` (for implication, also
 * written as →).
 *
 * Implication obeys the following laws:
 *
 *  - a → a = 1
 *  - a ∧ (a → b) = a ∧ b
 *  - b ∧ (a → b) = b
 *  - a → (b ∧ c) = (a → b) ∧ (a → c)
 *
 * In heyting algebras, `and` is equivalent to `meet` and `or` is
 * equivalent to `join`; both methods are available.
 *
 * Heyting algebra also define `complement` operation (sometimes
 * written as ¬a). The complement of `a` is equivalent to `(a → 0)`,
 * and the following laws hold:
 *
 *  - a ∧ ¬a = 0
 *
 * However, in Heyting algebras this operation is only a
 * pseudo-complement, since Heyting algebras do not necessarily
 * provide the law of the excluded middle. This means that there is no
 * guarantee that (a ∨ ¬a) = 1.
 *
 * Heyting algebras model intuitionistic logic. For a model of
 * classical logic, see the boolean algebra type class implemented as
 * `Bool`.
 */
trait Heyting[@sp(Int, Long) A] extends Any with BoundedDistributiveLattice[A] { self =>
  def and(a: A, b: A): A
  def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  def join(a: A, b: A): A = or(a, b)

  def imp(a: A, b: A): A
  def complement(a: A): A

  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = complement(xor(a, b))
}

trait HeytingGenBoolOverlap[H[A] <: Heyting[A]] {
  def and[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.and(x, y)
  def or[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.or(x, y)
  def xor[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.xor(x, y)
}

trait HeytingFunctions[H[A] <: Heyting[A]]
    extends BoundedMeetSemilatticeFunctions[H]
    with BoundedJoinSemilatticeFunctions[H] {

  def complement[@sp(Int, Long) A](x: A)(implicit ev: H[A]): A =
    ev.complement(x)

  def imp[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.imp(x, y)
  def nor[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nor(x, y)
  def nxor[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nxor(x, y)
  def nand[@sp(Int, Long) A](x: A, y: A)(implicit ev: H[A]): A =
    ev.nand(x, y)
}

object Heyting extends HeytingFunctions[Heyting] with HeytingGenBoolOverlap[Heyting] {

  /**
   * Access an implicit `Heyting[A]`.
   */
  @inline final def apply[@sp(Int, Long) A](implicit ev: Heyting[A]): Heyting[A] = ev
}
