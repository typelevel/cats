package cats

import scala.util.{Either, Left, Right}

/**
 * A mix-in for inheriting tailRecM on monads which define a stack-safe flatMap.  This is
 * ''not'' an appropriate trait to use unless you are 100% certain your monad is stack-safe
 * by definition!  If your monad is not stack-safe, then the tailRecM implementation you
 * will inherit will not be sound, and will result in unexpected stack overflows.  This
 * trait is only provided because a large number of monads ''do'' define a stack-safe
 * flatMap, and so this particular implementation was being repeated over and over again.
 *
 * Note, tailRecM being safe and pure implies that the function passed to flatMap
 * is not called immediately on the current stack (since otherwise tailRecM would
 * stack overflow for sufficiently deep recursions). This implies we can implement
 *
 * defer(fa) = unit.flatMap(_ => fa)
 */
trait StackSafeMonad[F[_]] extends Monad[F] with Defer[F] {

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

  /**
   * This is always safe for a StackSafeMonad.
   * proof: we know flatMap can't blow the stack
   * because if it could, tailRecM would not be safe:
   * if the function was called in the same stack then
   * the depth would diverse on tailRecM(())(_ => pure(Left(())))
   *
   * It may be better to override this for your particular Monad
   */
  def defer[A](fa: => F[A]): F[A] =
    flatMap(unit)(_ => fa)
}

object StackSafeMonad {
  def shiftFunctor[F[_], A, B](fn: A => F[B])(implicit F: Functor[F]): A => F[B] =
    F match {
      case ssm: StackSafeMonad[F] @unchecked => { a => ssm.defer(fn(a)) }
      case _                                 => fn
    }
}
