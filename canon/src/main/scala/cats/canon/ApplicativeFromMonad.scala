package cats
package canon

trait ApplicativeFromMonad[F[_]] extends Applicative[F] with FunctorFromApplicative[F] { self: Monad[F] =>
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f => flatMap(fa)(a => pure(f(a))))
}
