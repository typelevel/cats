package cats
package std

import cats.syntax.all._
import cats.data.Xor

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait TryInstances extends TryInstances1 {

  implicit def catsStdInstancesForTry: MonadError[Try, Throwable] with CoflatMap[Try] =
    new TryCoflatMap with MonadError[Try, Throwable]{
      def pure[A](x: A): Try[A] = Success(x)

      override def pureEval[A](x: Eval[A]): Try[A] = x match {
        case Now(x) => Success(x)
        case _ => Try(x.value)
      }

      override def product[A, B](ta: Try[A], tb: Try[B]): Try[(A, B)] = (ta, tb) match {
        case (Success(a), Success(b)) => Success((a, b))
        case (f: Failure[_], _) => f.asInstanceOf[Try[(A, B)]]
        case (_, f: Failure[_]) => f.asInstanceOf[Try[(A, B)]]
      }

      override def map2[A, B, Z](ta: Try[A], tb: Try[B])(f: (A, B) => Z): Try[Z] = (ta, tb) match {
        case (Success(a), Success(b)) => Try(f(a, b))
        case (f: Failure[_], _) => f.asInstanceOf[Try[Z]]
        case (_, f: Failure[_]) => f.asInstanceOf[Try[Z]]
      }

      override def map2Eval[A, B, Z](ta: Try[A], tb: Eval[Try[B]])(f: (A, B) => Z): Eval[Try[Z]] =
        ta match {
          case f: Failure[_] => Now(f.asInstanceOf[Try[Z]])
          case Success(a) => tb.map(_.map(f(a, _)))
        }

      def flatMap[A, B](ta: Try[A])(f: A => Try[B]): Try[B] = ta.flatMap(f)

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
