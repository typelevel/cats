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

/**
 * Inject is a type class providing an injection from type `A` into
 * type `B`. An injection is a function `inj` which does not destroy
 * any information: for every `b: B` there is at most one `a: A` such
 * that `inj(a) = b`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `b: B` back with a single value `a: A`.
 *
 * @since 1.0
 * @note Prior to cats 1.0, Inject handled injection for type
 * constructors. For injection of type constructors, use [[InjectK]].
 *
 * @see [[InjectK]] for injection for [[cats.data.EitherK]]
 */
abstract class Inject[A, B] {
  def inj: A => B

  def prj: B => Option[A]

  final def apply(a: A): B = inj(a)

  final def unapply(b: B): Option[A] = prj(b)
}

sealed abstract private[cats] class InjectInstances {
  implicit def catsReflexiveInjectInstance[A]: Inject[A, A] =
    new Inject[A, A] {
      val inj = identity(_: A)

      val prj: A => Option[A] = Some(_: A)
    }

  implicit def catsLeftInjectInstance[A, B]: Inject[A, Either[A, B]] =
    new Inject[A, Either[A, B]] {
      val inj: A => Either[A, B] = Left(_: A)

      val prj = (_: Either[A, B]).left.toOption
    }

  implicit def catsRightInjectInstance[A, B, C](implicit I: Inject[A, B]): Inject[A, Either[C, B]] =
    new Inject[A, Either[C, B]] {
      val inj: A => Either[C, B] = (a: A) => Right(I.inj(a))

      val prj = (_: Either[C, B]).toOption.flatMap(I.prj)
    }

}

object Inject extends InjectInstances {
  def apply[A, B](implicit I: Inject[A, B]): Inject[A, B] = I
}
