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
package instances

import TryInstances.castFailure

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.annotation.tailrec

trait TryInstances extends TryInstances1 {

  implicit def catsStdInstancesForTry: MonadThrow[Try] with CoflatMap[Try] with Traverse[Try] with Monad[Try] =
    new TryCoflatMap with MonadThrow[Try] with Traverse[Try] with Monad[Try] {
      def pure[A](x: A): Try[A] = Success(x)

      override def product[A, B](ta: Try[A], tb: Try[B]): Try[(A, B)] =
        (ta, tb) match {
          case (Success(a), Success(b)) => Success((a, b))
          case (f: Failure[?], _)       => castFailure[(A, B)](f)
          case (_, f: Failure[?])       => castFailure[(A, B)](f)
        }

      override def map2[A, B, Z](ta: Try[A], tb: Try[B])(f: (A, B) => Z): Try[Z] =
        (ta, tb) match {
          case (Success(a), Success(b)) => Try(f(a, b))
          case (f: Failure[?], _)       => castFailure[Z](f)
          case (_, f: Failure[?])       => castFailure[Z](f)
        }

      override def map2Eval[A, B, Z](ta: Try[A], tb: Eval[Try[B]])(f: (A, B) => Z): Eval[Try[Z]] =
        ta match {
          case f: Failure[?] => Now(castFailure[Z](f))
          case Success(a)    => tb.map(_.map(f(a, _)))
        }

      def flatMap[A, B](ta: Try[A])(f: A => Try[B]): Try[B] = ta.flatMap(f)

      def foldLeft[A, B](fa: Try[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Success(a) => f(b, a)
          case Failure(_) => b
        }

      def foldRight[A, B](fa: Try[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case Success(a) => f(a, lb)
          case Failure(_) => lb
        }

      def traverse[G[_], A, B](fa: Try[A])(f: A => G[B])(implicit G: Applicative[G]): G[Try[B]] =
        fa match {
          case Success(a)    => G.map(f(a))(Success(_))
          case f: Failure[?] => G.pure(castFailure[B](f))
        }

      override def mapAccumulate[S, A, B](init: S, fa: Try[A])(f: (S, A) => (S, B)): (S, Try[B]) = {
        fa match {
          case Success(a) =>
            val (snext, b) = f(init, a)
            (snext, Success(b))
          case f: Failure[?] => (init, castFailure[B](f))
        }
      }

      @tailrec final def tailRecM[B, C](b: B)(f: B => Try[Either[B, C]]): Try[C] =
        f(b) match {
          case f: Failure[?]     => castFailure[C](f)
          case Success(Left(b1)) => tailRecM(b1)(f)
          case Success(Right(c)) => Success(c)
        }

      def handleErrorWith[A](ta: Try[A])(f: Throwable => Try[A]): Try[A] =
        ta.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Try[A] = Failure(e)

      override def handleError[A](ta: Try[A])(f: Throwable => A): Try[A] =
        ta.recover { case t => f(t) }

      override def attempt[A](ta: Try[A]): Try[Either[Throwable, A]] =
        ta match { case Success(a) => Success(Right(a)); case Failure(e) => Success(Left(e)) }

      override def redeem[A, B](ta: Try[A])(recover: Throwable => B, map: A => B): Try[B] =
        ta match { case Success(a) => Try(map(a)); case Failure(e) => Try(recover(e)) }

      override def redeemWith[A, B](ta: Try[A])(recover: Throwable => Try[B], bind: A => Try[B]): Try[B] =
        try
          ta match {
            case Success(a) => bind(a); case Failure(e) => recover(e)
          }
        catch { case e if NonFatal(e) => Failure(e) }

      override def recover[A](ta: Try[A])(pf: PartialFunction[Throwable, A]): Try[A] =
        ta.recover(pf)

      override def recoverWith[A](ta: Try[A])(pf: PartialFunction[Throwable, Try[A]]): Try[A] = ta.recoverWith(pf)

      override def fromTry[A](t: Try[A])(implicit ev: Throwable <:< Throwable): Try[A] = t

      override def map[A, B](ta: Try[A])(f: A => B): Try[B] = ta.map(f)

      override def reduceLeftToOption[A, B](fa: Try[A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.map(f).toOption

      override def reduceRightToOption[A, B](fa: Try[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(fa.map(f).toOption)

      override def reduceLeftOption[A](fa: Try[A])(f: (A, A) => A): Option[A] =
        fa.toOption

      override def reduceRightOption[A](fa: Try[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
        Now(fa.toOption)

      override def get[A](fa: Try[A])(idx: Long): Option[A] =
        if (idx == 0L) fa.toOption else None

      override def size[A](fa: Try[A]): Long =
        if (fa.isSuccess) 1L else 0L

      override def find[A](fa: Try[A])(f: A => Boolean): Option[A] =
        fa.toOption.filter(f)

      override def foldMap[A, B](fa: Try[A])(f: A => B)(implicit B: Monoid[B]): B =
        fa match {
          case Failure(_) => B.empty
          case Success(a) => f(a)
        }

      override def exists[A](fa: Try[A])(p: A => Boolean): Boolean =
        fa match {
          case Failure(_) => false
          case Success(a) => p(a)
        }

      override def forall[A](fa: Try[A])(p: A => Boolean): Boolean =
        fa match {
          case Failure(_) => true
          case Success(a) => p(a)
        }

      override def toList[A](fa: Try[A]): List[A] =
        fa match {
          case Failure(_) => Nil
          case Success(a) => a :: Nil
        }

      override def isEmpty[A](fa: Try[A]): Boolean = fa.isFailure

      override def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< Throwable): Try[A] = Try(a)

      override def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< Throwable): Try[A] = Try(a.value)

      private[this] val successUnit: Try[Unit] = Success(())

      override def void[A](t: Try[A]): Try[Unit] =
        if (t.isSuccess) successUnit
        else t.asInstanceOf[Try[Unit]]

      override def unit: Try[Unit] = successUnit
    }

  implicit def catsStdShowForTry[A](implicit A: Show[A]): Show[Try[A]] = {
    case Success(a) => s"Success(${A.show(a)})"
    case Failure(e) => s"Failure($e)"
  }

  /**
   * you may wish to do equality by making `implicit val eqT: Eq[Throwable] = Eq.allEqual`
   * doing a fine grained equality on Throwable can make the code very execution
   * order dependent
   */
  implicit def catsStdEqForTry[A](implicit A: Eq[A], T: Eq[Throwable]): Eq[Try[A]] =
    Eq.catsStdEqForTry
}

private[instances] object TryInstances {

  /**
   * A `Failure` can be statically typed as `Try[A]` for all `A`, because it
   * does not actually contain an `A` value (as `Success[A]` does).
   */
  @inline final def castFailure[A](f: Failure[?]): Try[A] = f.asInstanceOf[Try[A]]
}

sealed private[instances] trait TryInstances1 extends TryInstances2 {
  implicit def catsStdMonoidForTry[A: Monoid]: Monoid[Try[A]] =
    new TryMonoid[A]
}

sealed private[instances] trait TryInstances2 {
  implicit def catsStdSemigroupForTry[A: Semigroup]: Semigroup[Try[A]] =
    new TrySemigroup[A]
}

abstract private[cats] class TryCoflatMap extends CoflatMap[Try] {
  def map[A, B](ta: Try[A])(f: A => B): Try[B] = ta.map(f)
  def coflatMap[A, B](ta: Try[A])(f: Try[A] => B): Try[B] = Try(f(ta))
}

private[cats] class TrySemigroup[A: Semigroup] extends ApplySemigroup[Try, A](try_.catsStdInstancesForTry, implicitly)

private[cats] class TryMonoid[A](implicit A: Monoid[A])
    extends ApplicativeMonoid[Try, A](try_.catsStdInstancesForTry, implicitly)
