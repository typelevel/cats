package cats
package syntax

trait BitraverseSyntax extends BitraverseSyntax1 {
  implicit final def catsSyntaxBitraverse[F[_, _]: Bitraverse, A, B](fab: F[A, B]): BitraverseOps[F, A, B] =
    new BitraverseOps[F, A, B](fab)
}

private[syntax] trait BitraverseSyntax1 {
  implicit final def catsSyntaxNestedBitraverse[F[_, _]: Bitraverse, G[_], A, B](
    fgagb: F[G[A], G[B]]
  ): NestedBitraverseOps[F, G, A, B] =
    new NestedBitraverseOps[F, G, A, B](fgagb)
}

final class BitraverseOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
  def bitraverse[G[_]: Applicative, C, D](f: A => G[C], g: B => G[D])(implicit F: Bitraverse[F]): G[F[C, D]] =
    F.bitraverse(fab)(f, g)
}

final class NestedBitraverseOps[F[_, _], G[_], A, B](private val fgagb: F[G[A], G[B]]) extends AnyVal {
  def bisequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    F.bisequence(fgagb)
}
