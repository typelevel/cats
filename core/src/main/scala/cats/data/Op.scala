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
 */
final case class Op[Arr[_, _], A, B](run: Arr[B, A]) {
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
