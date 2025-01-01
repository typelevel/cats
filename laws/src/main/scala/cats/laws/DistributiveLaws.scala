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

import cats.Id
import cats.data.Nested
import cats.syntax.distributive.*

trait DistributiveLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: Distributive[F]

  def distributeIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.distribute[Id, B](f) <-> F.map(fa)(f)

  def cosequenceIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.cosequence[Id, A](fa) <-> fa

  def cosequenceTwiceIsId[A, M[_]](fma: F[M[A]])(implicit M: Distributive[M]): IsEq[F[M[A]]] = {
    val result = F.cosequence(M.cosequence(fma))
    fma <-> result
  }

  def composition[A, B, C, M[_], N[_]](
    ma: M[A],
    f: A => F[B],
    g: B => N[C]
  )(implicit N: Distributive[N], M: Functor[M]): IsEq[Nested[F, N, M[C]]] = {
    val rhs = ma.distribute[Nested[F, N, *], C](a => Nested(F.map(f(a))(g)))
    val lhs = Nested(F.map(ma.distribute(f))(fb => fb.distribute(g)))
    lhs <-> rhs
  }
}

object DistributiveLaws {
  def apply[F[_]](implicit ev: Distributive[F]): DistributiveLaws[F] =
    new DistributiveLaws[F] { def F: Distributive[F] = ev }
}
