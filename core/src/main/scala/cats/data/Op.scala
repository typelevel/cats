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

package cats.data

import cats.kernel.Eq
import cats.arrow.Compose

/**
 * The `Op` class represents the dual of a morphism in category theory.
 *
 * In a normal category, arrows (or morphisms) have a direction `A => B`. 
 * The dual category reverses these arrows, making them `B => A`. 
 * The `Op` type is a simple wrapper that provides this “reversed” view.
 *
 * Practically, `Op` can be useful when you want to reason about or define
 * operations in terms of their duals without modifying the original category.
 * For example, when you have an existing `Compose` instance for a category,
 * `Op` allows you to flip the composition order and explore properties of
 * the dual structure.
 *
 * While `Compose` already defines both `compose` and `andThen` (and the `<<<` / `>>>` operators),
 * `Op` exists as a separate abstraction to explicitly capture and work with
 * the *dual category* concept in functional programming.
 *
 * Example:
 * {{{
 * import cats._
 * import cats.data._
 *
 * val f: Int => String = _.toString
 * val g: String => Double = _.length.toDouble
 *
 * val opF = Op(f)
 * val opG = Op(g)
 *
 * // Composition in Op reverses direction
 * val composed = opF.compose(opG) // equivalent to g andThen f
 * composed.run(1234) // 4.0
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
