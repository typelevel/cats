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

trait DecidableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decidable[F]

  def loseConsistency[A](f: A => Nothing): IsEq[F[A]] =
    F.lose(f) <-> F.contramap[Nothing, A](F.zero)(f)

  def decideConsistency[A, B, C](fa: F[A], fb: F[B], f: C => Either[A, B]): IsEq[F[C]] =
    F.decide(fa, fb)(f) <-> F.contramap(F.sum(fa, fb))(f)

  def decideRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide[Nothing, A, A](F.zero, fa)(Right.apply) <-> fa

  def decideLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide[A, Nothing, A](fa, F.zero)(Left.apply) <-> fa

  def decidableDecideLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide(fa, F.trivial[A])(Left.apply[A, A]) <-> fa

  def decidableDecideRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide(F.trivial[A], fa)(Right.apply[A, A]) <-> fa

  def decidableSumAssociativity[A, B, C](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  ): IsEq[F[Either[Either[A, B], C]]] =
    F.contramap[Either[A, Either[B, C]], Either[Either[A, B], C]](F.sum(fa, F.sum(fb, fc))) {
      case Left(Left(x))  => Left(x)
      case Left(Right(x)) => Right(Left(x))
      case Right(x)       => Right(Right(x))
    } <-> F.sum(F.sum(fa, fb), fc)

  def decidableRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    F.contramap(F.sum(fa, fb))((eit: Either[C, C]) =>
      eit.fold(
        f.andThen(Left.apply),
        g.andThen(Right.apply)
      )
    ) <-> F.sum(F.contramap(fa)(f), F.contramap(fb)(g))

  def decidableRightDistributivitySum[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, Either[B, C])]] =
    F.product(fa, F.sum(fb, fc)) <->
      F.contramap(F.sum(F.product(fa, fb), F.product(fa, fc))) {
        case (a, Left(b))  => Left((a, b))
        case (a, Right(c)) => Right((a, c))
      }
}

object DecidableLaws {
  def apply[F[_]](implicit ev: Decidable[F]): DecidableLaws[F] =
    new DecidableLaws[F] { def F: Decidable[F] = ev }
}
