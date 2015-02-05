package cats
package syntax

import cats.functor.Contravariant

trait ContravariantSyntax {
  implicit def invariantSyntax[F[_]: Contravariant, A](fa: F[A]) =
    new ContravariantOps(fa)
}

class ContravariantOps[F[_], A](fa: F[A])(implicit F: Contravariant[F]) {
  def contramap[B](f: B => A): F[B] = F.contramap(fa)(f)
}
