package cats.kernel
package instances

trait EitherInstances extends EitherInstances0 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdOrderForEither")
  private[instances] def catsStdOrderForEither[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdOrderForEither[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdMonoidForEither")
  private[instances] def catsDataMonoidForEither[A, B](implicit B: Monoid[B]): Monoid[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdMonoidForEither[A, B]

}

private[instances] trait EitherInstancesBinCompat0 extends EitherInstances0BinCompat0 {
  implicit def catsKernelStdOrderForEither[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] =
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

  implicit def catsKernelStdMonoidForEither[A, B](implicit B: Monoid[B]): Monoid[Either[A, B]] =
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

trait EitherInstances0 extends EitherInstances1 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdMonoidForEither")
  private[instances] def catsDataSemigroupForEither[A, B](implicit B: Semigroup[B]): Semigroup[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdSemigroupForEither[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdPartialOrderForEither")
  private[instances] def catsStdPartialOrderForEither[A, B](implicit A: PartialOrder[A],
                                                            B: PartialOrder[B]): PartialOrder[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdPartialOrderForEither[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdHashForEither")
  private[instances] def catsStdHashForEither[A, B](implicit A: Hash[A], B: Hash[B]): Hash[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdHashForEither[A, B]
}

private[instances] trait EitherInstances0BinCompat0 extends EitherInstances1BinCompat0 {

  implicit def catsKernelStdSemigroupForEither[A, B](implicit B: Semigroup[B]): Semigroup[Either[A, B]] =
    new Semigroup[Either[A, B]] {
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

  implicit def catsKernelStdPartialOrderForEither[A, B](implicit A: PartialOrder[A],
                                                        B: PartialOrder[B]): PartialOrder[Either[A, B]] =
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

  implicit def catsKernelStdHashForEither[A, B](implicit A: Hash[A], B: Hash[B]): Hash[Either[A, B]] =
    new EitherHash[A, B]
}

trait EitherInstances1 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.either.catsKernelStdHashForEither")
  private[instances] def catsStdEqForEither[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] =
    cats.kernel.instances.either.catsKernelStdEqForEither[A, B]

}

private[instances] trait EitherInstances1BinCompat0 {
  implicit def catsKernelStdEqForEither[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] = new EitherEq[A, B]
}

// isolated class for inheritance
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

class EitherHash[A, B](implicit A: Hash[A], B: Hash[B]) extends EitherEq[A, B] with Hash[Either[A, B]] {
  def hash(x: Either[A, B]): Int =
    x match {
      case Left(xx)  => StaticMethods.product1HashWithPrefix(A.hash(xx), "Left")
      case Right(xx) => StaticMethods.product1HashWithPrefix(B.hash(xx), "Right")
    }
}
