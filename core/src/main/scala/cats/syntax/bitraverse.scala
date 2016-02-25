package cats
package syntax

trait BitraverseSyntax {
  implicit def bitraverseSyntax[F[_, _]: Bitraverse, A, B](fab: F[A, B]): BitraverseOps[F, A, B] =
    new BitraverseOps[F, A, B](fab)
}

final class BitraverseOps[F[_, _], A, B](fab: F[A, B])(implicit F: Bitraverse[F]) {
  def bitraverse[G[_]: Applicative, C, D](f: A => G[C], g: B => G[D]): G[F[C, D]] =
    F.bitraverse(fab)(f, g)

  def sequence[G[_], C, D](implicit G: Applicative[G], evLeft: A =:= G[C], evRight: B =:= G[D]): G[F[C, D]] =
    F.bisequence(fab.asInstanceOf[F[G[C], G[D]]])
}
