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

/**
 * Laws that must be obeyed by any `cats.Parallel`.
 */
trait ParallelLaws[M[_]] extends NonEmptyParallelLaws[M] {
  val P: Parallel[M]

  def isomorphicPure[A](a: A): IsEq[F[A]] =
    P.applicative.pure(a) <-> P.parallel(P.monad.pure(a))
}

object ParallelLaws {
  type Aux[M[_], F0[_]] = ParallelLaws[M] { type F[A] = F0[A]; val P: Parallel.Aux[M, F0] }

  def apply[M[_]](implicit ev: Parallel[M]): ParallelLaws.Aux[M, ev.F] =
    apply[M, ev.F](ev, implicitly)

  def apply[M[_], F[_]](implicit ev: Parallel.Aux[M, F], D: DummyImplicit): ParallelLaws.Aux[M, F] =
    new ParallelLaws[M] { val P: ev.type = ev }
}
