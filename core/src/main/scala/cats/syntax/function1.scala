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
package syntax

trait Function1Syntax {

  implicit def catsSyntaxFunction1[F[_]: Functor, A, B](fab: F[Function1[A, B]]): Function1Ops[F, A, B] =
    new Function1Ops[F, A, B](fab)

  implicit def catsSyntaxFunction1FlatMap[F[_]: FlatMap, A, B](fab: Function1[A, F[B]]): Function1FlatMapOps[F, A, B] =
    new Function1FlatMapOps[F, A, B](fab)

  final class Function1Ops[F[_]: Functor, A, B](fab: F[Function1[A, B]]) {

    /**
     * Given a function in the Functor context and a plain value, supplies the
     * value to the function.
     *
     * Example:
     * {{{
     * scala> import cats.syntax.all._
     *
     * scala> val someF: Option[Int => Long] = Some(_.toLong + 1L)
     * scala> val noneF: Option[Int => Long] = None
     * scala> val anInt: Int = 3
     *
     * scala> someF.mapApply(anInt)
     * res0: Option[Long] = Some(4)
     *
     * scala> noneF.mapApply(anInt)
     * res1: Option[Long] = None
     *
     * }}}
     */
    def mapApply(a: A): F[B] = Functor[F].map(fab)(_(a))
  }

  final class Function1FlatMapOps[F[_]: FlatMap, A, B](f: A => F[B]) {

    /**
     * Alias for `a => f(a).flatMap(g)` or `(Kleisli(f) andThen Kleisli(g)).run`
     *
     * Example:
     * {{{
     * scala> import scala.util._
     * scala> import cats.syntax.all._
     *
     * scala> val f: List[String] => Option[String] = _.headOption
     * scala> val g: String => Option[Int] = str => Try(str.toInt).toOption
     * scala> (f >=> g)(List("42"))
     * res0: Option[Int] = Some(42)
     * scala> (f andThenF g)(List("abc"))
     * res1: Option[Int] = None
     * scala> (f andThenF g)(List())
     * res2: Option[Int] = None
     * }}}
     */
    def andThenF[C](g: B => F[C]): A => F[C] = a => FlatMap[F].flatMap(f(a))(g)

    /**
     * Alias for `f andThenF g`.
     */
    @inline def >=>[C](g: B => F[C]): A => F[C] = f.andThenF(g)

    /**
     * Alias for `c => g(c).flatMap(f)` or `(Kleisli(f) compose Kleisli(g)).run`
     *
     * Example:
     * {{{
     * scala> import scala.util._
     * scala> import cats.syntax.all._
     *
     * scala> val f: String => Option[Int] = str => Try(str.toInt).toOption
     * scala> val g: List[String] => Option[String] = _.headOption
     * scala> (f composeF g)(List("42"))
     * res0: Option[Int] = Some(42)
     * scala> (f composeF g)(List("abc"))
     * res1: Option[Int] = None
     * scala> (f composeF g)(List())
     * res2: Option[Int] = None
     * }}}
     */
    def composeF[C](g: C => F[A]): C => F[B] = c => FlatMap[F].flatMap(g(c))(f)

    /**
     * Alias for `f composeF g`.
     */
    @inline def <=<[C](g: C => F[A]): C => F[B] = f.composeF(g)
  }
}
