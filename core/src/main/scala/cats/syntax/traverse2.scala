package cats
package syntax

trait Traverse2Syntax extends Traverse2Syntax1 {
  implicit def catsSyntaxTraverse2[F[_, _]: Traverse2, A, B](fab: F[A, B]): Traverse2Ops[F, A, B] =
    new Traverse2Ops[F, A, B](fab)
}

private[syntax] trait Traverse2Syntax1 {
  implicit def catsSyntaxNestedTraverse2[F[_, _]: Traverse2, G[_], A, B](fgagb: F[G[A], G[B]]): NestedTraverse2Ops[F, G, A, B] =
    new NestedTraverse2Ops[F, G, A, B](fgagb)
}

final class Traverse2Ops[F[_, _], A, B](fab: F[A, B])(implicit F: Traverse2[F]) {
  def traverse2[G[_]: Applicative, C, D](f: A => G[C], g: B => G[D]): G[F[C, D]] =
    F.traverse2(fab)(f, g)
}

final class NestedTraverse2Ops[F[_, _], G[_], A, B](fgagb: F[G[A], G[B]])(implicit F: Traverse2[F]) {
  def sequence2(implicit G: Applicative[G]): G[F[A, B]] =
    F.sequence2(fgagb)
}
