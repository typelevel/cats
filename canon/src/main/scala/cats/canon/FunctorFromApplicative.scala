package cats
package canon

trait FunctorFromApplicative[F[_]] extends Functor[F] { self: Applicative[F] =>
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}

