package cats
package canon

trait CanonicalApplicativeFromMonad[F[_]] extends Applicative[F] with CanonicalFunctorFromApplicative[F] { self: Monad[F] =>
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f => map(fa)(f))
}

object CanonicalApplicativeFromMonad {
  def apply[F[_]](implicit F: Monad[F]): Applicative[F] = new Monad[F] with CanonicalApplicativeFromMonad[F] {
    override def pure[A](x: A): F[A] = F.pure(x)
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
  }
}