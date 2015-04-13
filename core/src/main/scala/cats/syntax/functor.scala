package cats
package syntax

trait FunctorSyntax {
  // TODO: use simulacrum instances eventually
  implicit def functorSyntax[FA](fa: FA)(implicit U: Unapply[Functor,FA]): FunctorOps[U.M, U.A] =
    new FunctorOps[U.M, U.A](U.subst(fa))(U.TC)
}

class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
  def map[B](f: A => B): F[B] = F.map(fa)(f)
  def void: F[Unit] = F.void(fa)
  def fproduct[B](f: A => B): F[(A, B)] = F.fproduct(fa)(f)
  def as[B](b: B): F[B] = F.as(fa, b)
}
