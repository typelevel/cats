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

trait BitraverseSyntaxBinCompat0 {
  implicit final def catsSyntaxBitraverseBinCompat0[F[_, _]: Bitraverse, A, B](
    fab: F[A, B]
  ): BitraverseOpsBinCompat0[F, A, B] =
    new BitraverseOpsBinCompat0[F, A, B](fab)
  implicit final def catsSyntaxLeftNestedBitraverse[F[_, _]: Bitraverse, G[_], A, B](
    fgab: F[G[A], B]
  ): LeftNestedBitraverseOps[F, G, A, B] =
    new LeftNestedBitraverseOps[F, G, A, B](fgab)
  implicit final def catsSyntaxRightnestedBitraverse[F[_, _]: Bitraverse, G[_], A, B](
    fagb: F[A, G[B]]
  ): RightNestedBitraverseOps[F, G, A, B] =
    new RightNestedBitraverseOps[F, G, A, B](fagb)
}

final class BitraverseOpsBinCompat0[F[_, _], A, B](val fab: F[A, B]) extends AnyVal {
  def rightTraverse[G[_], C](f: B => G[C])(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, C]] =
    F.bitraverse(fab)(G.pure(_), f)
  def leftTraverse[G[_], C](f: A => G[C])(implicit F: Bitraverse[F], G: Applicative[G]): G[F[C, B]] =
    F.bitraverse(fab)(f, G.pure(_))
}

final class LeftNestedBitraverseOps[F[_, _], G[_], A, B](val fgab: F[G[A], B]) extends AnyVal {
  def leftSequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    F.bitraverse(fgab)(identity, G.pure(_))
}

final class RightNestedBitraverseOps[F[_, _], G[_], A, B](val fagb: F[A, G[B]]) extends AnyVal {
  def rightSequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    F.bitraverse(fagb)(G.pure(_), identity)
}
