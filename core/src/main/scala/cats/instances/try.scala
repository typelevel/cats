package cats
package instances

import TryInstances.castFailure

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.annotation.tailrec

trait TryInstances extends TryInstances1 {

  // scalastyle:off method.length
  implicit def catsStdInstancesForTry
    : MonadError[Try, Throwable] with CoflatMap[Try] with Traverse[Try] with Monad[Try] =
    new TryCoflatMap with MonadError[Try, Throwable] with Traverse[Try] with Monad[Try] {
      def pure[A](x: A): Try[A] = Success(x)

      override def product[A, B](ta: Try[A], tb: Try[B]): Try[(A, B)] = (ta, tb) match {
        case (Success(a), Success(b)) => Success((a, b))
        case (f: Failure[_], _)       => castFailure[(A, B)](f)
        case (_, f: Failure[_])       => castFailure[(A, B)](f)
      }

      override def map2[A, B, Z](ta: Try[A], tb: Try[B])(f: (A, B) => Z): Try[Z] = (ta, tb) match {
        case (Success(a), Success(b)) => Try(f(a, b))
        case (f: Failure[_], _)       => castFailure[Z](f)
        case (_, f: Failure[_])       => castFailure[Z](f)
      }

      override def map2Eval[A, B, Z](ta: Try[A], tb: Eval[Try[B]])(f: (A, B) => Z): Eval[Try[Z]] =
        ta match {
          case f: Failure[_] => Now(castFailure[Z](f))
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
          case f: Failure[_] => G.pure(castFailure[B](f))
        }

      @tailrec final def tailRecM[B, C](b: B)(f: B => Try[Either[B, C]]): Try[C] =
        f(b) match {
          case f: Failure[_]     => castFailure[C](f)
          case Success(Left(b1)) => tailRecM(b1)(f)
          case Success(Right(c)) => Success(c)
        }

      def handleErrorWith[A](ta: Try[A])(f: Throwable => Try[A]): Try[A] =
        ta.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Try[A] = Failure(e)

      override def handleError[A](ta: Try[A])(f: Throwable => A): Try[A] =
        ta.recover { case t => f(t) }

      override def attempt[A](ta: Try[A]): Try[Either[Throwable, A]] =
        (ta.map(a => Right[Throwable, A](a))).recover { case NonFatal(t) => Left(t) }

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
        fa match {
          case Failure(_) => 0L
          case Success(_) => 1L
        }

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
        case _                        => false
      }
    }
}

private[instances] object TryInstances {

  /**
   * A `Failure` can be statically typed as `Try[A]` for all `A`, because it
   * does not actually contain an `A` value (as `Success[A]` does).
   */
  @inline final def castFailure[A](f: Failure[_]): Try[A] = f.asInstanceOf[Try[A]]
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
