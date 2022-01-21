package cats
package syntax

trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] =
    new MonadOps(fa)

  implicit final def catsSyntaxMonadIdOps[A](a: A): MonadIdOps[A] =
    new MonadIdOps[A](a)

  implicit final def catsSyntaxMonadBooleanOps[F[_]](fa: F[Boolean]): MonadBooleanOps[F] =
    new MonadBooleanOps[F](fa)
}

final class MonadIdOps[A](private val a: A) extends AnyVal {

  /**
   * Iterative application of `f` while `p` holds.
   */
  def iterateWhileM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhileM(a)(f)(p)

  /**
   * Iterative application of `f` until `p` holds.
   */
  def iterateUntilM[F[_]](f: A => F[A])(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntilM(a)(f)(p)
}

final class MonadBooleanOps[F[_]](private val fa: F[Boolean]) extends AnyVal {

  /**
   * Behaves like `&&` but inside the `F` context.
   *
   * Wont evaluate `other` unless this evaluates to `true`
   */
  def andM(other: => F[Boolean])(implicit F: Monad[F]): F[Boolean] =
    F.flatMap(fa) {
      case true  => other
      case false => F.pure(false)
    }

  /**
   * Behaves like `||` but inside the `F` context.
   *
   * Wont evaluate `other` unless this evaluates to `false`
   */
  def orM(other: => F[Boolean])(implicit F: Monad[F]): F[Boolean] =
    F.flatMap(fa) {
      case true  => F.pure(false)
      case false => other
    }
}
