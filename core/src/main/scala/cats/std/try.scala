package cats
package std

import cats.syntax.all._
import cats.data.Xor
import TryInstances.castFailure

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait TryInstances extends TryInstances1 {

  // scalastyle:off method.length
  implicit def catsStdInstancesForTry: MonadError[Try, Throwable] with CoflatMap[Try] with Traverse[Try] =
    new TryCoflatMap with MonadError[Try, Throwable] with Traverse[Try] {
      def pure[A](x: A): Try[A] = Success(x)

      override def pureEval[A](x: Eval[A]): Try[A] = x match {
        case Now(x) => Success(x)
        case _ => Try(x.value)
      }

      override def product[A, B](ta: Try[A], tb: Try[B]): Try[(A, B)] = (ta, tb) match {
        case (Success(a), Success(b)) => Success((a, b))
        case (f: Failure[_], _) => castFailure[(A, B)](f)
        case (_, f: Failure[_]) => castFailure[(A, B)](f)
      }

      override def map2[A, B, Z](ta: Try[A], tb: Try[B])(f: (A, B) => Z): Try[Z] = (ta, tb) match {
        case (Success(a), Success(b)) => Try(f(a, b))
        case (f: Failure[_], _) => castFailure[Z](f)
        case (_, f: Failure[_]) => castFailure[Z](f)
      }

      override def map2Eval[A, B, Z](ta: Try[A], tb: Eval[Try[B]])(f: (A, B) => Z): Eval[Try[Z]] =
        ta match {
          case f: Failure[_] => Now(castFailure[Z](f))
          case Success(a) => tb.map(_.map(f(a, _)))
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
          case Success(a) => G.map(f(a))(Success(_))
          case f: Failure[_] => G.pure(castFailure[B](f))
        }

      def handleErrorWith[A](ta: Try[A])(f: Throwable => Try[A]): Try[A] =
        ta.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Try[A] = Failure(e)
      override def handleError[A](ta: Try[A])(f: Throwable => A): Try[A] =
        ta.recover { case t => f(t) }

      override def attempt[A](ta: Try[A]): Try[Throwable Xor A] =
        (ta map Xor.right) recover { case NonFatal(t) => Xor.left(t) }

      override def recover[A](ta: Try[A])(pf: PartialFunction[Throwable, A]): Try[A] =
        ta.recover(pf)

      override def recoverWith[A](ta: Try[A])(pf: PartialFunction[Throwable, Try[A]]): Try[A] = ta.recoverWith(pf)

      override def map[A, B](ta: Try[A])(f: A => B): Try[B] = ta.map(f)
    }
  // scalastyle:on method.length

  implicit def catsStdShowForTry[A](implicit A: Show[A]): Show[Try[A]] =
    new Show[Try[A]] {
      def show(fa: Try[A]): String = fa match {
        case Success(a) => s"Success(${A.show(a)})"
        case Failure(e) => s"Failure($e)"
      }
    }
  /**
   * you may wish to do equality by making `implicit val eqT: Eq[Throwable] = Eq.allEqual`
   * doing a fine grained equality on Throwable can make the code very execution
   * order dependent
   */
  implicit def catsStdEqForTry[A, T](implicit A: Eq[A], T: Eq[Throwable]): Eq[Try[A]] =
    new Eq[Try[A]] {
      def eqv(x: Try[A], y: Try[A]): Boolean = (x, y) match {
        case (Success(a), Success(b)) => A.eqv(a, b)
        case (Failure(a), Failure(b)) => T.eqv(a, b)
        case _ => false
      }
    }
}

private[std] object TryInstances {
  /**
   * A `Failure` can be statically typed as `Try[A]` for all `A`, because it
   * does not actually contain an `A` value (as `Success[A]` does).
   */
  @inline final def castFailure[A](f: Failure[_]): Try[A] = f.asInstanceOf[Try[A]]
}

private[std] sealed trait TryInstances1 extends TryInstances2 {
  implicit def catsStdMonoidForTry[A: Monoid]: Monoid[Try[A]] =
    new TryMonoid[A]
}

private[std] sealed trait TryInstances2 {
  implicit def catsStdSemigroupForTry[A: Semigroup]: Semigroup[Try[A]] =
    new TrySemigroup[A]
}

private[cats] abstract class TryCoflatMap extends CoflatMap[Try] {
  def map[A, B](ta: Try[A])(f: A => B): Try[B] = ta.map(f)
  def coflatMap[A, B](ta: Try[A])(f: Try[A] => B): Try[B] = Try(f(ta))
}

private[cats] class TrySemigroup[A: Semigroup] extends Semigroup[Try[A]] {
  def combine(fx: Try[A], fy: Try[A]): Try[A] =
    for {
      x <- fx
      y <- fy
    } yield x |+| y
}

private[cats] class TryMonoid[A](implicit A: Monoid[A]) extends TrySemigroup[A] with Monoid[Try[A]] {
  def empty: Try[A] = Success(A.empty)
}
