package cats

import scala.util.{Either, Left, Right}

/**
 * A mix-in for inheriting tailRecM on monads which define a stack-safe flatMap.  This is
 * ''not'' an appropriate trait to use unless you are 100% certain your monad is stack-safe
 * by definition!  If your monad is not stack-safe, then the tailRecM implementation you
 * will inherit will not be sound, and will result in unexpected stack overflows.  This
 * trait is only provided because a large number of monads ''do'' define a stack-safe
 * flatMap, and so this particular implementation was being repeated over and over again.
 */
trait StackSafeMonad[F[_]] extends Monad[F] {

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }
}
