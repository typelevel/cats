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
package laws

import cats.data.Cokleisli
import cats.syntax.all._

/**
 * Laws that must be obeyed by any `CoflatMap`.
 */
trait CoflatMapLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: CoflatMap[F]

  def coflatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): IsEq[F[C]] =
    fa.coflatMap(f).coflatMap(g) <-> fa.coflatMap(x => g(x.coflatMap(f)))

  def coflattenThroughMap[A](fa: F[A]): IsEq[F[F[F[A]]]] =
    fa.coflatten.coflatten <-> fa.coflatten.map(_.coflatten)

  def coflattenCoherence[A, B](fa: F[A], f: F[A] => B): IsEq[F[B]] =
    fa.coflatMap(f) <-> fa.coflatten.map(f)

  def coflatMapIdentity[A, B](fa: F[A]): IsEq[F[F[A]]] =
    fa.coflatten <-> fa.coflatMap(identity)

  /**
   * The composition of `cats.data.Cokleisli` arrows is associative. This is
   * analogous to [[coflatMapAssociativity]].
   */
  def cokleisliAssociativity[A, B, C, D](f: F[A] => B, g: F[B] => C, h: F[C] => D, fa: F[A]): IsEq[D] = {
    val (cf, cg, ch) = (Cokleisli(f), Cokleisli(g), Cokleisli(h))
    cf.andThen(cg).andThen(ch).run(fa) <-> cf.andThen(cg.andThen(ch)).run(fa)
  }
}

object CoflatMapLaws {
  def apply[F[_]](implicit ev: CoflatMap[F]): CoflatMapLaws[F] =
    new CoflatMapLaws[F] { def F: CoflatMap[F] = ev }
}
