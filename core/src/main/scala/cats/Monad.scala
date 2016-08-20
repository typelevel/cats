package cats

import simulacrum.typeclass

/**
 * Monad.
 *
 * Allows composition of dependent effectful functions.
 *
 * See: [[http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf Monads for functional programming]]
 *
 * Must obey the laws defined in cats.laws.MonadLaws.
 */
@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  /**
   * This is not stack safe if the monad is not trampolined, but it
   * is always lawful. It it better if you can find a stack safe way
   * to write this method (all cats types have a stack safe version
   * of this). When this method is safe you can find an `implicit r: RecursiveTailRecM`.
   */
  protected def defaultTailRecM[A, B](a: A)(fn: A => F[Either[A, B]]): F[B] =
    flatMap(fn(a)) {
      case Right(b) => pure(b)
      case Left(nextA) => defaultTailRecM(nextA)(fn)
    }
}
