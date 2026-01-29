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

package cats
package data

import cats.arrow.*

/**
 * The dual category of some other category, `Arr`.
 *
 * In a normal category, `Arr` has a direction `A => B`.
 * The dual category reverses the direction, making it `B => A`.
 *
 * The dual category can be useful when you want to reason about or define
 * operations in terms of their duals without modifying the original category.
 * In other words, the dual category provides a "reversed" view to the original category.
 *
 * Example:
 * {{{
 * import cats.arrow.Compose
 * import cats.data.Op
 * import cats.syntax.all.*
 *
 * val f: String => String = a => s"f($a)"
 * val g: String => String = b => s"g($b)"
 *
 * // `>>>` is an alias for `andThen`
 * (f >>> g)("a")
 * // res0: String = g(f(a))
 *
 * // `Op` reverses the composition
 * (Op(f) >>> Op(g)).run("a")
 * // res1: String = f(g(a))
 * }}}
 */
final case class Op[Arr[_, _], A, B](run: Arr[B, A]) {

  /**
   * Compose two `Op` values. Note that composition order is reversed compared to `Arr`.
   *
   * Example:
   * {{{
   * import cats._
   * import cats.data._
   *
   * val f: Int => String = _.toString
   * val g: String => Int = _.length
   *
   * val opF = Op(f)
   * val opG = Op(g)
   *
   * val composed = opF.compose(opG)
   * // composed: cats.data.Op[Function1, Int, Int]
   *
   * composed.run(1234)
   * // res0: Int = 4
   * }}}
   */
  def compose[Z](op: Op[Arr, Z, A])(implicit Arr: Compose[Arr]): Op[Arr, Z, B] =
    Op(Arr.compose(op.run, run))

  def eqv(op: Op[Arr, A, B])(implicit Arr: Eq[Arr[B, A]]): Boolean =
    Arr.eqv(run, op.run)
}

object Op extends OpInstances

sealed abstract private[data] class OpInstances extends OpInstances0 {
  implicit def catsDataCategoryForOp[Arr[_, _]](implicit ArrC: Category[Arr]): Category[Op[Arr, *, *]] =
    new OpCategory[Arr] { def Arr: Category[Arr] = ArrC }

  implicit def catsDataEqForOp[Arr[_, _], A, B](implicit ArrEq: Eq[Arr[B, A]]): Eq[Op[Arr, A, B]] =
    new OpEq[Arr, A, B] { def Arr: Eq[Arr[B, A]] = ArrEq }

  @deprecated("Use catsDataEqForOp", "2.0.0-RC2")
  def catsKernelEqForOp[Arr[_, _], A, B](implicit ArrEq: Eq[Arr[B, A]]): Eq[Op[Arr, A, B]] =
    catsDataEqForOp[Arr, A, B]
}

sealed abstract private[data] class OpInstances0 {
  implicit def catsDataComposeForOp[Arr[_, _]](implicit ArrC: Compose[Arr]): Compose[Op[Arr, *, *]] =
    new OpCompose[Arr] { def Arr: Compose[Arr] = ArrC }
}

private[data] trait OpCategory[Arr[_, _]] extends Category[Op[Arr, *, *]] with OpCompose[Arr] {
  implicit def Arr: Category[Arr]

  override def id[A]: Op[Arr, A, A] = Op(Arr.id)
}

private[data] trait OpCompose[Arr[_, _]] extends Compose[Op[Arr, *, *]] {
  implicit def Arr: Compose[Arr]

  def compose[A, B, C](f: Op[Arr, B, C], g: Op[Arr, A, B]): Op[Arr, A, C] =
    f.compose(g)
}

private[data] trait OpEq[Arr[_, _], A, B] extends Eq[Op[Arr, A, B]] {
  implicit def Arr: Eq[Arr[B, A]]

  def eqv(f: Op[Arr, A, B], g: Op[Arr, A, B]): Boolean =
    f.eqv(g)
}
