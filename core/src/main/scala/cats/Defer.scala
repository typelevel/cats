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

import scala.util.control.TailCalls.TailRec

/**
 * Defer is a type class that shows the ability to defer creation
 * inside of the type constructor F[_].
 *
 * This comes up with F[_] types that are implemented with a trampoline
 * or are based on function application.
 *
 * The law is that defer(fa) is equivalent to fa, but not evaluated immediately,
 * so
 * {{{
 * scala> import cats._
 * scala> import cats.implicits._
 *
 * scala> var evaluated = false
 * scala> val dfa = Defer[Eval].defer {
 *      |   evaluated = true
 *      |   Eval.now(21)
 *      | }
 *
 * scala> evaluated
 * res0: Boolean = false
 *
 * scala> Eq[Eval[Int]].eqv(dfa, Eval.now(21))
 * res1: Boolean = true
 * }}}
 */
trait Defer[F[_]] extends Serializable {
  def defer[A](fa: => F[A]): F[A]

  /**
   * Defer instances, like functions, parsers, generators, IO, etc...
   * often are used in recursive settings where this function is useful
   *
   * fix(fn) == fn(fix(fn))
   *
   * example:
   *
   * val parser: P[Int] =
   *   Defer[P].fix[Int] { rec =>
   *     CharsIn("0123456789") | P("(") ~ rec ~ P(")")
   *   }
   *
   * Note, fn may not yield a terminating value in which case both
   * of the above F[A] run forever.
   */
  def fix[A](fn: F[A] => F[A]): F[A] = {
    lazy val res: F[A] = defer(fn(res))
    res
  }
}

object Defer {
  def apply[F[_]](implicit defer: Defer[F]): Defer[F] = defer

  implicit def catsDeferForFunction0: Defer[Function0] = cats.instances.function.catsSddDeferForFunction0
  implicit def catsDeferForFunction1[A]: Defer[Function1[A, *]] = cats.instances.function.catsStdDeferForFunction1[A]
  implicit def catsDeferForTailRec: Defer[TailRec] = cats.instances.tailRec.catsInstancesForTailRec
}
