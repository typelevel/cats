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

trait BitraverseSyntaxBinCompat0 extends BisequenceSyntaxBinCompat0 {
  implicit final def catsSyntaxBitraverseBinCompat0[F[_, _], A, B](fab: F[A, B]): BitraverseOpsBinCompat0[F, A, B] =
    new BitraverseOpsBinCompat0(fab)
}

final class BitraverseOpsBinCompat0[F[_, _], A, B](private val value: F[A, B]) extends AnyVal {

  /**
   * Traverse the left side of the structure with the given function.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   *
   * scala> ("1", "2").leftTraverse(parseInt)
   * res0: Option[(Int, String)] = Some((1,2))
   *
   * scala> ("two", "2").leftTraverse(parseInt)
   * res2: Option[(Int, String)] = None
   *
   * }}}
   */
  def leftTraverse[G[_], C](f: A => G[C])(implicit F: Bitraverse[F], G: Applicative[G]): G[F[C, B]] =
    F.bitraverse(value)(f, G.pure)
}

trait BisequenceSyntaxBinCompat0 {
  implicit final def catsSyntaxBisequenceBinCompat0[F[_, _], G[_], A, B](
    fgab: F[G[A], B]
  ): BisequenceOpsBinCompat0[F, G, A, B] =
    new BisequenceOpsBinCompat0(fgab)
}

final class BisequenceOpsBinCompat0[F[_, _], G[_], A, B](private val value: F[G[A], B]) extends AnyVal {
  def leftSequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    new BitraverseOpsBinCompat0(value).leftTraverse(identity)
}
