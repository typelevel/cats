package cats.kernel
package instances

package object either extends EitherInstances

trait EitherInstances extends EitherInstances0 {

  implicit def catsStdOrderForEither[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] =
    new Order[Either[A, B]] {
      def compare(x: Either[A, B], y: Either[A, B]): Int =
        x match {
          case Left(xx) => y match {
            case Left(yy) => A.compare(xx, yy)
            case Right(_) => -1
          }
          case Right(xx) => y match {
            case Left(_) => 1
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
          case Right(xx) => y match {
            case left @ Left(_) => left
            case Right(yy) => Right(B.combine(xx, yy))
          }
        }
    }
}

trait EitherInstances0 extends EitherInstances1 {

  implicit def catsDataSemigroupForEither[A, B](implicit B: Semigroup[B]): Semigroup[Either[A, B]] =
    new Semigroup[Either[A, B]] {
      def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] =
        x match {
          case left @ Left(_) => left
          case Right(xx) => y match {
            case left @ Left(_) => left
            case Right(yy) => Right(B.combine(xx, yy))
          }
        }
    }

  implicit def catsStdPartialOrderForEither[A, B](implicit A: PartialOrder[A], B: PartialOrder[B]): PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double =
        x match {
          case Left(xx) => y match {
            case Left(yy) => A.partialCompare(xx, yy)
            case Right(_) => -1.0
          }
          case Right(xx) => y match {
            case Left(_) => 1.0
            case Right(yy) => B.partialCompare(xx, yy)
          }
        }
    }
}

trait EitherInstances1 {
  implicit def catsStdEqForEither[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] =
    new Eq[Either[A, B]] {
      def eqv(x: Either[A, B], y: Either[A, B]): Boolean =
        x match {
          case Left(xx) => y match {
            case Left(yy) => A.eqv(xx, yy)
            case Right(_) => false
          }
          case Right(xx) => y match {
            case Left(_) => false
            case Right(yy) => B.eqv(xx, yy)
          }
        }
    }
}
