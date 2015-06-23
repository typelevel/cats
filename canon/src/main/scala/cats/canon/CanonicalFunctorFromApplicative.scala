package cats
package canon

trait CanonicalFunctorFromApplicative[F[_]] extends Functor[F] { self: Applicative[F] =>
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}

object CanonicalFunctorFromApplicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = new Applicative[F] with CanonicalFunctorFromApplicative[F] {
    override def pure[A](x: A): F[A] = F.pure(x)
    override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = F.ap(fa)(f)
  }
}
