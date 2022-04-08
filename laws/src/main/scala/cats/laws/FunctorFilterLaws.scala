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

package cats
package laws

trait FunctorFilterLaws[F[_]] {
  implicit def F: FunctorFilter[F]

  implicit def functor: Functor[F] = F.functor

  def mapFilterComposition[A, B, C](fa: F[A], f: A => Option[B], g: B => Option[C]): IsEq[F[C]] = {
    val lhs: F[C] = F.mapFilter(F.mapFilter(fa)(f))(g)
    val rhs: F[C] = F.mapFilter(fa)(a => f(a).flatMap(g))
    lhs <-> rhs
  }

  def mapFilterMapConsistency[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    F.mapFilter(fa)(f.andThen(x => Some(x): Option[B])) <-> functor.map(fa)(f)

  def collectConsistentWithMapFilter[A, B](fa: F[A], f: PartialFunction[A, B]): IsEq[F[B]] =
    F.collect(fa)(f) <-> F.mapFilter(fa)(f.lift)

  def flattenOptionConsistentWithMapFilter[A](fa: F[Option[A]]): IsEq[F[A]] =
    F.flattenOption(fa) <-> F.mapFilter(fa)(identity)

  def filterConsistentWithMapFilter[A](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    F.filter(fa)(f) <->
      F.mapFilter(fa)(a => if (f(a)) Some(a) else None)

  def filterNotConsistentWithFilter[A](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    F.filterNot(fa)(f) <-> F.filter(fa)(!f(_))
}

object FunctorFilterLaws {
  def apply[F[_]](implicit ev: FunctorFilter[F]): FunctorFilterLaws[F] =
    new FunctorFilterLaws[F] { def F: FunctorFilter[F] = ev }
}
