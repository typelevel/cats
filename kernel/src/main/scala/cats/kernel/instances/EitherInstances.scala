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

package cats.kernel
package instances

trait EitherInstances extends EitherInstances0 {

  implicit def catsStdOrder2AndHash2ForEither: Order2[Either] with Hash2[Either] =
    new Order2[Either] with Hash2[Either] {
      override def liftCompare2[A, B, C, D](compareAB: (A, B) => Int,
                                            compareCD: (C, D) => Int,
                                            x: Either[A, C],
                                            y: Either[B, D]
      ): Int =
        (x, y) match {
          case (Left(x), Left(y)) =>
            compareAB(x, y)
          case (Right(x), Right(y)) =>
            compareCD(x, y)
          case (Left(_), _) =>
            -1
          case _ =>
            1
        }

      override def liftHash2[A, B](hashA: A => Int, hashB: B => Int, x: Either[A, B]): Int =
        x match {
          case Left(xx)  => StaticMethods.product1HashWithPrefix(hashA(xx), "Left")
          case Right(xx) => StaticMethods.product1HashWithPrefix(hashB(xx), "Right")
        }
    }

  @deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
  def catsStdOrderForEither[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] =
    new Order[Either[A, B]] {
      def compare(x: Either[A, B], y: Either[A, B]): Int =
        x match {
          case Left(xx) =>
            y match {
              case Left(yy) => A.compare(xx, yy)
              case Right(_) => -1
            }
          case Right(xx) =>
            y match {
              case Left(_)   => 1
              case Right(yy) => B.compare(xx, yy)
            }
        }
    }

  implicit def catsDataMonoidForEither[A, B](implicit B: Monoid[B]): Monoid[Either[A, B]] =
    new Monoid[Either[A, B]] {
      def empty: Either[A, B] =
        Right(B.empty)
      def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] =
        x match {
          case left @ Left(_) => left
          case Right(xx) =>
            y match {
              case left @ Left(_) => left
              case Right(yy)      => Right(B.combine(xx, yy))
            }
        }
    }
}

private[instances] trait EitherInstances0 extends EitherInstances1 {

  implicit def catsDataSemigroupForEither[A, B](implicit B: Semigroup[B]): Semigroup[Either[A, B]] =
    (x, y) =>
      x match {
        case left @ Left(_) => left
        case Right(xx) =>
          y match {
            case left @ Left(_) => left
            case Right(yy)      => Right(B.combine(xx, yy))
          }
      }

  @deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
  def catsStdPartialOrderForEither[A, B](implicit
    A: PartialOrder[A],
    B: PartialOrder[B]
  ): PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double =
        x match {
          case Left(xx) =>
            y match {
              case Left(yy) => A.partialCompare(xx, yy)
              case Right(_) => -1.0
            }
          case Right(xx) =>
            y match {
              case Left(_)   => 1.0
              case Right(yy) => B.partialCompare(xx, yy)
            }
        }
    }

  @deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
  def catsStdHashForEither[A, B](implicit A: Hash[A], B: Hash[B]): Hash[Either[A, B]] = Hash[Either[A, B]]
}

private[instances] trait EitherInstances1 {

  @deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
  def catsStdEqForEither[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] = Eq[Either[A, B]]
}

// isolated class for inheritance
@deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
class EitherEq[A, B](implicit A: Eq[A], B: Eq[B]) extends Eq[Either[A, B]] {
  def eqv(x: Either[A, B], y: Either[A, B]): Boolean =
    x match {
      case Left(xx) =>
        y match {
          case Left(yy) => A.eqv(xx, yy)
          case Right(_) => false
        }
      case Right(xx) =>
        y match {
          case Left(_)   => false
          case Right(yy) => B.eqv(xx, yy)
        }
    }
}

@deprecated(message = "Please use catsStdOrder2AndHash2ForEither", since = "2.8.0")
class EitherHash[A, B](implicit A: Hash[A], B: Hash[B]) extends EitherEq[A, B] with Hash[Either[A, B]] {
  def hash(x: Either[A, B]): Int =
    x match {
      case Left(xx)  => StaticMethods.product1HashWithPrefix(A.hash(xx), "Left")
      case Right(xx) => StaticMethods.product1HashWithPrefix(B.hash(xx), "Right")
    }
}
