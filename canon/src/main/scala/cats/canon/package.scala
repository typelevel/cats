package cats

package object canon {
  def canonicalApplicative[F[_]](implicit F: Applicative[F]): Applicative[F] =
    new Applicative[F] with FunctorFromApplicative[F] {
      override def pure[A](x: A): F[A] = F.pure(x)
      override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = F.ap(fa)(f)
    }
  def canonicalMonad[F[_]](implicit F: Monad[F]): Monad[F] =
    new Monad[F] with ApplicativeFromMonad[F] {
      override def pure[A](x: A): F[A] = F.pure(x)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    }
}
