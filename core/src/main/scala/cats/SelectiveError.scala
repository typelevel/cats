package cats

import cats.syntax.either._

trait SelectiveError[F[_], E] extends ApplicativeError[F, E] with Selective[F] {

  /**
   * Turns a successful value into an error if it does not satisfy a given predicate.
   */
  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] =
    select(map(fa) { a: A =>
      if (predicate(a)) Right(a)
      else Either.leftUnit
    })(raiseError(error))
}
